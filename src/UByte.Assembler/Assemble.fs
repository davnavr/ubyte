module UByte.Assembler.Assemble

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open UByte.Helpers

open FParsec

open UByte.Format.Model

open UByte.Assembler.Parser

type AssemblerError =
    | ParserError of string * ParserError
    | ValidationError of string * Position

    override this.ToString() =
        match this with
        | ParserError(msg, _) -> msg
        | ValidationError(msg, pos) -> sprintf "%s %O" msg pos

[<Sealed>]
type SymbolDictionary<'IndexKind, 'T when 'IndexKind :> IndexKinds.Kind> () =
    let items = ImmutableArray.CreateBuilder<'T>()
    let symbols = Dictionary<Name, int32>()

    member _.Count = items.Count

    member _.Values = items :> IReadOnlyCollection<'T>

    member _.AddAnonymous item: Index<'IndexKind> =
        let i = items.Count
        items.Add item
        Index(uint32 i)

    member this.AddNamed(id, item) =
        let (exists, _) = symbols.TryGetValue id
        if not exists then
            let (Index i) as index = this.AddAnonymous item
            symbols.Add(id, Checked.int32 i)
            ValueSome index
        else
            ValueNone

    member this.Add(id: Name voption, item) =
        match id with
        | ValueSome symbol -> this.AddNamed(symbol, item)
        | ValueNone -> ValueSome(this.AddAnonymous item)

    member this.Add(symbol: Symbol voption, item) =
        let id' =
            match symbol with
            | ValueSome(_, id) -> ValueSome id
            | ValueNone -> ValueNone

        this.Add(id', item)

    member _.FromIdentifier id: Index<'IndexKind> voption =
        match symbols.TryGetValue id with
        | true, item -> ValueSome(Index(Checked.uint32 item))
        | false, _ -> ValueNone

    member _.ToVector(): vector<'T> = items.ToImmutable()

    member _.ValueAt i = items.[i]

let assemble declarations = // TODO: Fix, use result so at least one error object is always returned. Using voption means forgetting to add to the error list results in no error message.
    let errors = ImmutableArray.CreateBuilder<AssemblerError>()
    let mutable fversion, mname, mversion, mmain = ValueNone, ValueNone, ValueNone, ValueNone
    let identifiers = SymbolDictionary<IndexKinds.Identifier, string>()
    let namespaces = SymbolDictionary<IndexKinds.Namespace, Symbol list>()
    let emptyIdentifierIndex = identifiers.AddAnonymous String.Empty
    let tsignatures = SymbolDictionary<IndexKinds.TypeSignature, ParsedTypeSignature>()
    let msignatures = SymbolDictionary<IndexKinds.MethodSignature, _>()
    let mdimports = SymbolDictionary<IndexKinds.Kind, _>()
    let timports = SymbolDictionary<IndexKinds.Kind, _>()
    let tdefinitions = SymbolDictionary<IndexKinds.Kind, _>()
    let data = SymbolDictionary<IndexKinds.Data, _>()
    let codes = SymbolDictionary<IndexKinds.Code, _>()
    let emptyNamespaceIndex = namespaces.AddAnonymous List.empty

    let inline addValidationError msg pos = errors.Add(AssemblerError.ValidationError(msg, pos))

    let addLookupValue (lookup: SymbolDictionary<_, _>) (struct(pos, id)) value dup =
        if lookup.AddNamed(id, value).IsNone then
            addValidationError (dup id) pos

    let duplicateSymbolMessage kind: Name -> _ = sprintf "%s declaration corresponding to the symbol @%O already exists" kind

    let findLookupValue (lookup: SymbolDictionary<_, _>) err (struct(pos, id)) =
        match lookup.FromIdentifier id with
        | ValueSome i -> ValueSome i
        | ValueNone ->
            addValidationError (err id) pos
            ValueNone

    let mapLookupValues (lookup: SymbolDictionary<'IndexKind, _>) (mapping: Index<'IndexKind> -> 'T -> 'U voption) =
        let mutable items, success = Array.zeroCreate lookup.Count, true
        for i = 0 to items.Length - 1 do
            match mapping (Index (uint32 i)) (lookup.ValueAt i) with
            | ValueSome value when success -> items.[i] <- value
            | _ -> success <- false
        if success then
            ValueSome(Unsafe.As<'U[], ImmutableArray<'U>> &items)
        else
            ValueNone

    let lookupValueList (lookup: SymbolDictionary<_, _>) =
        let values = ImmutableArray.CreateBuilder()

        let rec inner symbols success =
            match symbols with
            | [] when success -> ValueSome(values.ToImmutable())
            | [] -> ValueNone
            | struct(pos, id) :: remaining ->
                let success' =
                    match lookup.FromIdentifier id with
                    | ValueSome i when success ->
                        values.Add i
                        true
                    | ValueSome _ ->
                        false
                    | ValueNone ->
                        addValidationError (sprintf "Unresolved symbol @%O" id) pos
                        false

                inner remaining success'

        fun symbols ->
            values.Clear()
            inner symbols true

    let lookupDefinitionOrImport imports definitions err symbol =
        match findLookupValue imports err symbol with
        | ValueSome(Index i) -> ValueSome(Index i)
        | ValueNone -> voptional {
            let! (Index i) = findLookupValue definitions err symbol
            return Index(Checked.(+) (uint32 imports.Count) i)
        }

    let rec inner declarations =
        match declarations with
        | [] ->
            if mname.IsNone then
                failwith "Missing module declaration, declare the name of the module with .module MyName"

            // TODO: Resolve all of the things
            let namespaces' = mapLookupValues namespaces (fun _ -> lookupValueList identifiers)

            let inline undefinedSymbolMessage name: Name -> _ =
                sprintf "%s corresponding to the symbol @%O could not be found" name

            let lookupID = findLookupValue identifiers (undefinedSymbolMessage "An identifier")

            let lookupDefinedType =
                lookupDefinitionOrImport
                    timports
                    tdefinitions
                    (undefinedSymbolMessage "A type definition")

            let tsignatures' =
                mapLookupValues tsignatures <| fun _ t ->
                    match t lookupDefinedType with
                    | Result.Ok t' -> ValueSome t'
                    | Result.Error(pos, missing) ->
                        addValidationError (sprintf "@%O" missing) pos
                        ValueNone

            let msignatures' =
                let resolveTypeSignatures = lookupValueList tsignatures
                mapLookupValues msignatures <| fun _ (rtypes, ptypes) -> voptional {
                    let! rtypes = resolveTypeSignatures rtypes
                    let! ptypes = resolveTypeSignatures ptypes
                    return { MethodSignature.ReturnTypes = rtypes; ParameterTypes = ptypes }
                }

            let mimports' =
                let rec resolveModuleDeclarations success name version declarations =
                    match declarations with
                    | [] when success ->
                        match name, version with
                        | ValueSome name', ValueSome version' ->
                            ValueSome { ModuleIdentifier.ModuleName = name'; Version = version' }
                        | _ -> failwith "TODO: Get position for error when module import name or version is missing"
                    | [] -> ValueNone
                    | ModuleImportDecl.Name(pos, name') :: remaining ->
                        match name with
                        | ValueNone -> resolveModuleDeclarations success (ValueSome name') version remaining
                        | ValueSome _ ->
                            addValidationError "Duplicate name declaration in module import" pos
                            resolveModuleDeclarations false name version remaining
                    | ModuleImportDecl.Version(pos, version') :: remaining ->
                        match version with
                        | ValueNone -> resolveModuleDeclarations success name (ValueSome version') remaining
                        | ValueSome _ ->
                            addValidationError "Duplicate version declaration in module import" pos
                            resolveModuleDeclarations false name version remaining

                mapLookupValues mdimports <| fun _ -> resolveModuleDeclarations true ValueNone ValueNone

            let lookupModuleImport symbol =
                voptional {
                    let! (Index i) =
                        findLookupValue
                            mdimports
                            (undefinedSymbolMessage "A module import")
                            symbol
                    return ModuleIndex.Index(Checked.(+) 1u i)
                }

            let inline duplicateVisibilityFlag desc pos = addValidationError ("Duplicate visibility flag in " + desc) pos

            let resolveFieldImport = // TODO: Avoid code duplication with resolveFieldDefinition
                let rec resolveFieldAttributes flags attributes =
                    match attributes with
                    | [] -> ValueSome flags
                    | FieldImportAttr.Flag(_, flag) :: remaining -> resolveFieldAttributes (flags ||| flag) remaining

                let rec resolveFieldDeclarations success flags name ftype declarations =
                    match declarations with
                    | [] when success -> ValueSome(name, ftype)
                    | [] -> ValueNone
                    | FieldImportDecl.Name((pos, _) as id) :: remaining ->
                        match name with
                        | ValueNone ->
                            match lookupID id with
                            | ValueSome _ as namei ->
                                resolveFieldDeclarations success flags namei ftype remaining
                            | ValueNone ->
                                resolveFieldDeclarations false flags name ftype remaining
                        | ValueSome _ ->
                            addValidationError "Duplicate name declaration in field import" pos
                            resolveFieldDeclarations false flags name ftype remaining
                    | FieldImportDecl.Type((pos, _) as id) :: remaining ->
                        match ftype with
                        | ValueNone ->
                            match findLookupValue tsignatures (undefinedSymbolMessage "A type definition") id with
                            | ValueSome _ as typei -> resolveFieldDeclarations success flags name typei remaining
                            | ValueNone -> resolveFieldDeclarations false flags name ftype remaining
                        | ValueSome _ ->
                            addValidationError "Duplicate type declaration in field import" pos
                            resolveFieldDeclarations false flags name ftype remaining

                fun owner attrs decls -> voptional {
                    let! flags = resolveFieldAttributes FieldFlags.ReadOnly attrs

                    match! resolveFieldDeclarations true flags ValueNone ValueNone decls with
                    | name, ValueSome ftype ->
                        return
                            { FieldImport.FieldOwner = owner
                              FieldName = ValueOption.defaultValue emptyIdentifierIndex name
                              FieldType = ftype }
                    | _, ValueNone ->
                        failwith "TODO: How to get position for field import type missing error"
                }

            let resolveMethodImport =  // TODO: Avoid code duplication with resolveMethodDefinition
                let rec resolveMethodAttributes flags attributes =
                    match attributes with
                    | [] -> ValueSome flags
                    | MethodImportAttr.Flag(_, flag) :: remaining ->
                        resolveMethodAttributes (flags ||| flag) remaining

                let rec resolveMethodDeclarations success flags name signature declarations =
                    match declarations with
                    | [] when success -> ValueSome(name, signature)
                    | [] -> ValueNone
                    | MethodImportDecl.Name((pos, _) as id) :: remaining ->
                        match name with
                        | ValueNone ->
                            match lookupID id with
                            | ValueSome _ as namei ->
                                resolveMethodDeclarations success flags namei signature remaining
                            | ValueNone ->
                                resolveMethodDeclarations false flags name signature remaining
                        | ValueSome _ ->
                            addValidationError "Duplicate name declaration in method definition" pos
                            resolveMethodDeclarations false flags name signature remaining
                    | MethodImportDecl.Signature((pos, _) as id) :: remaining ->
                        match signature with
                        | ValueNone ->
                            match findLookupValue msignatures (undefinedSymbolMessage "A method signature") id with
                            | ValueSome _ as signature' ->
                                resolveMethodDeclarations success flags name signature' remaining
                            | ValueNone ->
                                resolveMethodDeclarations false flags name signature remaining
                        | ValueSome _ ->
                            addValidationError "Duplicate method signature declaration in method definition" pos
                            resolveMethodDeclarations false flags name signature remaining

                fun owner attrs decls -> voptional {
                    let! flags = resolveMethodAttributes MethodFlags.Final attrs
                    let! (name, signature) = resolveMethodDeclarations true flags ValueNone ValueNone decls

                    match signature with
                    | ValueSome signature' ->
                        return
                            { MethodImport.MethodOwner = owner
                              MethodName = ValueOption.defaultValue emptyIdentifierIndex name
                              //MethodFlags = flags
                              TypeParameters = 0u
                              Signature = signature' }
                    | ValueNone ->
                        failwith "TODO: How to get position for missing signature in method import"
                }

            let fimports = SymbolDictionary<IndexKinds.Kind, _>()
            let mimports = SymbolDictionary<IndexKinds.Kind, _>()

            let timports' =
                let rec resolveTypeDeclarations owner declarations =
                    // TODO: Avoid code duplication with tdefinitions'
                    let rec inner success m ns name declarations =
                        match declarations with
                        | [] when success -> ValueSome(m, ns, name)
                        | [] -> ValueNone
                        | TypeImportDecl.Field((pos, id), fattrs, fdecls) :: remaining ->
                            match resolveFieldImport owner fattrs fdecls with
                            | ValueSome field ->
                                let success' =
                                    match fimports.AddNamed(id, field) with
                                    | ValueSome _ -> success
                                    | ValueNone ->
                                        addValidationError (duplicateSymbolMessage "A field import" id) pos
                                        false
                                inner success' m ns name remaining
                            | ValueNone ->
                                inner false m ns name remaining
                        | TypeImportDecl.Method((pos, id), mattrs, mdecls) :: remaining ->
                            match resolveMethodImport owner mattrs mdecls with
                            | ValueSome method ->
                                let success' =
                                    match mimports.AddNamed(id, method) with
                                    | ValueSome _ -> success
                                    | ValueNone ->
                                        addValidationError (duplicateSymbolMessage "A method import" id) pos
                                        false
                                inner success' m ns name remaining
                            | ValueNone ->
                                inner false m ns name remaining
                        | TypeImportDecl.Module((pos, _) as id) :: remaining ->
                            match name with
                            | ValueNone ->
                                match lookupModuleImport id with
                                | ValueSome _ as m' -> inner success m' ns name remaining
                                | ValueNone -> inner false m ns name remaining
                            | ValueSome _ ->
                                addValidationError "Duplicate module declaration in type import" pos
                                inner false m ns name remaining
                        | TypeImportDecl.Name((pos, _) as id) :: remaining ->
                            match name with
                            | ValueNone ->
                                match lookupID id with
                                | ValueSome _ as namei -> inner success m ns namei remaining
                                | ValueNone -> inner false m ns name remaining
                            | ValueSome _ ->
                                addValidationError "Duplicate name declaration in type import" pos
                                inner false m ns name remaining
                        | TypeImportDecl.Namespace(pos, id) :: remaining ->
                            match ns with
                            | ValueNone ->
                                match namespaces.FromIdentifier id with
                                | ValueSome _ as id' ->
                                    inner success m id' name remaining
                                | ValueNone ->
                                    addValidationError (undefinedSymbolMessage "A namespace" id) pos
                                    inner false m ns name remaining
                            | ValueSome _ ->
                                addValidationError "Duplicate namespace declaration in type import" pos
                                inner false m ns name remaining

                    inner true ValueNone ValueNone ValueNone declarations

                mapLookupValues timports <| fun (Index owner) decls -> voptional {
                    let! (mindex, ns, name) = resolveTypeDeclarations (TypeDefinitionIndex.Index owner) decls
                    return
                        { TypeDefinitionImport.Module = ValueOption.defaultValue (Index 0u) mindex
                          TypeName = ValueOption.defaultValue emptyIdentifierIndex name
                          TypeNamespace = ValueOption.defaultValue emptyNamespaceIndex ns
                          TypeParameters = 0u }
                }

            let resolveFieldDefinition =
                let rec resolveFieldAttributes success hasvis visibility flags attributes =
                    match attributes with
                    | [] when success -> ValueSome(visibility, flags)
                    | [] -> ValueNone
                    | FieldDefAttr.Visibility(pos, flag) :: remaining ->
                        if not hasvis then
                            resolveFieldAttributes success true flag flags remaining
                        else
                            duplicateVisibilityFlag "field definition" pos
                            resolveFieldAttributes false true visibility flags remaining
                    | FieldDefAttr.Flag(_, flag) :: remaining ->
                        resolveFieldAttributes success hasvis visibility (flags ||| flag) remaining

                let rec resolveFieldDeclarations success visibility flags name ftype declarations =
                    match declarations with
                    | [] when success -> ValueSome(name, ftype)
                    | [] -> ValueNone
                    | FieldDefDecl.Name((pos, _) as id) :: remaining ->
                        match name with
                        | ValueNone ->
                            match lookupID id with
                            | ValueSome _ as namei ->
                                resolveFieldDeclarations success visibility flags namei ftype remaining
                            | ValueNone ->
                                resolveFieldDeclarations false visibility flags name ftype remaining
                        | ValueSome _ ->
                            addValidationError "Duplicate name declaration in field definition" pos
                            resolveFieldDeclarations false visibility flags name ftype remaining
                    | FieldDefDecl.Type((pos, _) as id) :: remaining ->
                        match ftype with
                        | ValueNone ->
                            match findLookupValue tsignatures (undefinedSymbolMessage "A type definition") id with
                            | ValueSome _ as typei -> resolveFieldDeclarations success visibility flags name typei remaining
                            | ValueNone -> resolveFieldDeclarations false visibility flags name ftype remaining
                        | ValueSome _ ->
                            addValidationError "Duplicate type declaration in field definition" pos
                            resolveFieldDeclarations false visibility flags name ftype remaining

                fun owner attrs decls -> voptional {
                    let! (visibility, flags) =
                        resolveFieldAttributes true false VisibilityFlags.Unspecified FieldFlags.ReadOnly attrs

                    match! resolveFieldDeclarations true visibility flags ValueNone ValueNone decls with
                    | name, ValueSome ftype ->
                        return fun adjust ->
                            { Field.FieldOwner = adjust owner
                              FieldName = ValueOption.defaultValue emptyIdentifierIndex name
                              FieldVisibility = visibility
                              FieldFlags = flags
                              FieldType = ftype
                              FieldAnnotations = ImmutableArray.Empty }
                    | _, ValueNone ->
                        failwith "TODO: How to get position for field type missing error"
                }

            let resolveMethodDefinition =
                let rec resolveMethodAttributes success hasvis visibility flags attributes =
                    match attributes with
                    | [] when success -> ValueSome(visibility, flags)
                    | [] -> ValueNone
                    | MethodDefAttr.Visibility(pos, flag) :: remaining ->
                        if not hasvis then
                            resolveMethodAttributes success true flag flags remaining
                        else
                            duplicateVisibilityFlag "method definition" pos
                            resolveMethodAttributes false true visibility flags remaining
                    | MethodDefAttr.Flag(_, flag) :: remaining ->
                        resolveMethodAttributes success hasvis visibility (flags ||| flag) remaining

                let methodBodyLookup = findLookupValue codes (undefinedSymbolMessage "A method body")

                let rec resolveMethodDeclarations success visibility flags name signature body declarations =
                    match declarations with
                    | [] when success -> ValueSome(name, signature, body)
                    | [] -> ValueNone
                    | MethodDefDecl.Name((pos, _) as id) :: remaining ->
                        match name with
                        | ValueNone ->
                            match lookupID id with
                            | ValueSome _ as namei ->
                                resolveMethodDeclarations success visibility flags namei signature body remaining
                            | ValueNone ->
                                resolveMethodDeclarations false visibility flags name signature body remaining
                        | ValueSome _ ->
                            addValidationError "Duplicate name declaration in method definition" pos
                            resolveMethodDeclarations false visibility flags name signature body remaining
                    | MethodDefDecl.Body(pos, body') :: remaining ->
                        match body with
                        | ValueNone ->
                            let body'' =
                                match body' with
                                | ParsedMethodBody.Defined definition -> definition methodBodyLookup
                                | ParsedMethodBody.External external -> external lookupID

                            match body'' with
                            | Result.Ok body'' ->
                                resolveMethodDeclarations success visibility flags name signature (ValueSome body'') remaining
                            | Result.Error _ ->
                                resolveMethodDeclarations false visibility flags name signature body remaining
                        | ValueSome _ ->
                            addValidationError "Duplicate body declaration in method definition" pos
                            resolveMethodDeclarations false visibility flags name signature body remaining
                    | MethodDefDecl.Signature((pos, _) as id) :: remaining ->
                        match signature with
                        | ValueNone ->
                            match findLookupValue msignatures (undefinedSymbolMessage "A method signature") id with
                            | ValueSome _ as signature' ->
                                resolveMethodDeclarations success visibility flags name signature' body remaining
                            | ValueNone ->
                                resolveMethodDeclarations false visibility flags name signature body remaining
                        | ValueSome _ ->
                            addValidationError "Duplicate method signature declaration in method definition" pos
                            resolveMethodDeclarations false visibility flags name signature body remaining

                fun owner attrs decls -> voptional {
                    let! (visibility, flags) =
                        resolveMethodAttributes true false VisibilityFlags.Unspecified MethodFlags.Final attrs

                    let! (name, signature, body) =
                        resolveMethodDeclarations true visibility flags ValueNone ValueNone ValueNone decls

                    match signature with
                    | ValueSome signature' ->
                        return fun adjust ->
                            { Method.MethodOwner = adjust owner
                              MethodName = ValueOption.defaultValue emptyIdentifierIndex name
                              MethodVisibility = visibility
                              MethodFlags = flags
                              TypeParameters = ImmutableArray.Empty
                              Signature = signature'
                              MethodAnnotations = ImmutableArray.Empty
                              Body = ValueOption.defaultValue MethodBody.Abstract body }
                    | ValueNone ->
                        failwith "TODO: How to get position for missing signature in method definition"
                }

            let fdefinitions = SymbolDictionary<IndexKinds.Kind, _>()
            let mdefinitions = SymbolDictionary<IndexKinds.Kind, _>()

            let iresolver =
                { new IInstructionResolver with
                    member _.FindField symbol =
                        lookupDefinitionOrImport
                            fimports
                            fdefinitions
                            (undefinedSymbolMessage "A field definition or import")
                            symbol

                    member _.FindMethod symbol =
                        lookupDefinitionOrImport
                            mimports
                            mdefinitions
                            (undefinedSymbolMessage "A method definition or import")
                            symbol

                    member _.FindTypeSignature symbol =
                        findLookupValue tsignatures (undefinedSymbolMessage "A type signature") symbol

                    member _.FindData symbol =
                        findLookupValue data (undefinedSymbolMessage "A data") symbol }

            let tdefinitions' =
                let fields = ImmutableArray.CreateBuilder<uvarint>() // TODO: Store the index of the first method and keep track of the number of methods instead.
                let methods = ImmutableArray.CreateBuilder<uvarint>()
                let inherited = ImmutableArray.CreateBuilder()
                let overrides = ImmutableArray.CreateBuilder()

                let rec resolveTypeAttributes attributes =
                    let rec inner success hasvis visibility flags attributes =
                        match attributes with
                        | [] when success -> ValueSome(struct(visibility, flags))
                        | [] -> ValueNone
                        | TypeDefAttr.Visibility(pos, flag) :: remaining ->
                            if not hasvis then
                                inner success true flag flags remaining
                            else
                                duplicateVisibilityFlag "type definition" pos
                                inner false true visibility flags remaining
                        | TypeDefAttr.Flag(_, flag) :: remaining ->
                            inner success hasvis visibility (flags ||| flag) remaining
                    inner true false VisibilityFlags.Unspecified TypeDefinitionFlags.Final attributes

                let rec resolveTypeDeclarations owner declarations =
                    // TODO: Could have a lookup for field and method names here

                    let rec inner success ns name declarations =
                        match declarations with
                        | [] when success -> ValueSome(ns, name)
                        | [] -> ValueNone
                        | TypeDefDecl.Field(id, fattrs, fdecls) :: remaining ->
                            match resolveFieldDefinition owner fattrs fdecls with
                            | ValueSome field ->
                                let success' =
                                    match fdefinitions.Add(id, field) with
                                    | ValueSome(Index i) ->
                                        fields.Add i
                                        success
                                    | ValueNone ->
                                        false
                                inner success' ns name remaining
                            | ValueNone ->
                                inner false ns name remaining
                        | TypeDefDecl.Method(id, mattrs, mdecls) :: remaining ->
                            match resolveMethodDefinition owner mattrs mdecls with
                            | ValueSome method ->
                                let success' =
                                    match mdefinitions.Add(id, method) with
                                    | ValueSome(Index i) ->
                                        methods.Add i
                                        success
                                    | ValueNone -> false
                                inner success' ns name remaining
                            | ValueNone ->
                                inner false ns name remaining
                        | TypeDefDecl.Name((pos, _) as id) :: remaining ->
                            match name with
                            | ValueNone ->
                                match lookupID id with
                                | ValueSome _ as namei -> inner success ns namei remaining
                                | ValueNone -> inner false ns name remaining
                            | ValueSome _ ->
                                addValidationError "Duplicate name declaration in type definition" pos
                                inner false ns name remaining
                        | TypeDefDecl.Namespace(pos, id) :: remaining ->
                            match ns with
                            | ValueNone ->
                                match namespaces.FromIdentifier id with
                                | ValueSome _ as id' ->
                                    inner success id' name remaining
                                | ValueNone ->
                                    addValidationError (undefinedSymbolMessage "A namespace" id) pos
                                    inner false ns name remaining
                            | ValueSome _ ->
                                addValidationError "Duplicate namespace declaration in type definition" pos
                                inner false ns name remaining
                        | TypeDefDecl.Extends super :: remaining ->
                            let result = voptional {
                                let! superi = lookupDefinedType super
                                return inherited.Add superi
                            }

                            inner result.IsSome ns name remaining
                        | TypeDefDecl.MethodOverride(impl, decl) :: remaining ->
                            overrides.Add <| fun mstart -> voptional {
                                let! (Index decl') =
                                    findLookupValue mdefinitions (undefinedSymbolMessage "A method definition") decl
                                let! impl' = iresolver.FindMethod impl
                                return
                                    { MethodOverride.Declaration = MethodIndex.Index(Checked.(+) mstart decl')
                                      Implementation = impl' }
                            }

                            inner true ns name remaining

                    inner true ValueNone ValueNone declarations

                let resolveMethodOverrides (overrides: ImmutableArray<_ -> _>) mstart =
                    let mutable overrides', success = Array.zeroCreate overrides.Length, true

                    for i = 0 to overrides'.Length - 1 do
                        match overrides.[i] mstart with
                        | ValueSome moverride when success ->
                            overrides'.[i] <- moverride
                        | _ ->
                            success <- false

                    if success
                    then ValueSome(Unsafe.As<MethodOverride[], ImmutableArray<MethodOverride>> &overrides')
                    else ValueNone

                mapLookupValues tdefinitions <| fun (Index owner) (attrs, decls) -> voptional {
                    fields.Clear()
                    methods.Clear()
                    inherited.Clear()
                    overrides.Clear()
                    let! (visibility, flags) = resolveTypeAttributes attrs
                    let! (ns, name) = resolveTypeDeclarations owner decls
                    let fields', methods', inherited' = fields.ToImmutable(), methods.ToImmutable(), inherited.ToImmutable()
                    let overrides' = overrides.ToImmutable()

                    let inline adjust (start: uint32) (members: ImmutableArray<uvarint>) =
                        let mutable members' = Array.zeroCreate members.Length
                        for i = 0 to members'.Length - 1 do members'.[i] <- Index(Checked.(+) start members.[i])
                        Unsafe.As<Index<_>[], ImmutableArray<Index<_>>> &members'

                    return fun fstart mstart -> voptional {
                        let! vtable = resolveMethodOverrides overrides' mstart
                        return
                            { TypeDefinition.TypeName = ValueOption.defaultValue emptyIdentifierIndex name
                              TypeNamespace = ValueOption.defaultValue emptyNamespaceIndex ns
                              TypeVisibility = visibility
                              TypeFlags = flags
                              TypeLayout = TypeDefinitionLayout.Unspecified
                              TypeParameters = ImmutableArray.Empty
                              InheritedTypes = inherited'
                              TypeAnnotations = ImmutableArray.Empty
                              Fields = adjust fstart fields'
                              Methods = adjust mstart methods'
                              VTable = vtable }
                    }
                }

            let codes' =
                let resolveTypeSignature =
                    findLookupValue tsignatures (sprintf "A type signature corresponding to the symbol @%O could not be found")

                let resolveArgumentRegisters =
                    let rec inner index (lookup: Dictionary<Name, uint32>) registers success =
                        let index' = Checked.(+) 1u index
                        match registers with
                        | [] when success -> ValueSome lookup
                        | [] -> ValueNone
                        | ValueSome(struct(pos, name)) :: remaining ->
                            let success' = lookup.TryAdd(name, index)
                            if not success' then
                                addValidationError
                                    (sprintf "An argument register corresponding to the symbol $%O already exists" name)
                                    pos
                            inner index' lookup remaining success'
                        | ValueNone :: remaining ->
                            inner index' lookup remaining success

                    fun registers -> inner 0u (Dictionary()) registers true

                let resolveLocalRegisters =
                    let rec inner locals (lookup: Dictionary<Name, uint32>) success =
                        match locals with
                        | [] when success -> ValueSome lookup
                        | [] -> ValueNone
                        | struct(pos, name) :: remaining ->
                            let success' = lookup.TryAdd(name, uint32 lookup.Count)
                            if not success' then
                                addValidationError
                                    (sprintf "A local register corresponding to the symbol $%O already exists" name)
                                    pos
                            inner remaining lookup success'

                    fun locals -> inner locals (Dictionary()) true

                let ierrors = List<InvalidInstructionError> 0 :> InstructionErrorsBuilder

                let resolveCodeInstructions =
                    let instrs = ImmutableArray.CreateBuilder<InstructionSet.Instruction>()
                    let lmapping = ImmutableArray.CreateBuilder<struct(_ * _)>()
                    let temps = Dictionary<Name, TemporaryIndex>()

                    fun body blocki (blocks: Dictionary<_, _>) (locals: Dictionary<_, _>) (arguments: Dictionary<_, _>) ->
                        instrs.Clear()
                        lmapping.Clear()
                        temps.Clear()

                        let mutable success, index, tempi = true, 0, 0u

                        let inline registerNotFound id = ierrors.Add(InvalidInstructionError.UndefinedRegister id)

                        let blockRegisterResolver ({ ParsedRegister.Name = (_, name) as id } as register) =
                            if register.IsTemporary then
                                match temps.TryGetValue name with
                                | true, (Index i) -> ValueSome(RegisterIndex.Index i)
                                | false, _ ->
                                    registerNotFound id
                                    ValueNone
                            else
                                match arguments.TryGetValue name with
                                | true, i ->
                                    Checked.(+) tempi i
                                    |> RegisterIndex.Index
                                    |> ValueSome
                                | false, _ ->
                                    match locals.TryGetValue name with
                                    | true, i ->
                                        Checked.(+) tempi i
                                        |> Checked.(+) (uint32 arguments.Count)
                                        |> RegisterIndex.Index
                                        |> ValueSome
                                    | false, _ ->
                                        registerNotFound id
                                        ValueNone

                        let blockOffsetLookup (struct(_, name) as id) =
                            match blocks.TryGetValue name with
                            | true, struct(index, _) ->
                                int64 index - int64 blocki
                                |> Checked.int32
                                |> ValueSome
                            | false, _ ->
                                ierrors.Add(InvalidInstructionError.UndefinedBlock id)
                                ValueNone

                        List.iter
                            (fun struct(results: ParsedRegister list, instruction: ParsedInstruction) ->
                                match instruction blockRegisterResolver iresolver ierrors blockOffsetLookup with
                                | ValueSome instruction' -> instrs.Add instruction'
                                | ValueNone -> success <- false

                                List.iter
                                    (function
                                    | { ParsedRegister.IsTemporary = true; ParsedRegister.Name = pos, name } ->
                                        if not(temps.TryAdd(name, TemporaryIndex.Index tempi)) then
                                            addValidationError
                                                (sprintf "A temporary register corresponding to the symbol %%%O already exists" name)
                                                pos
                                            success <- false
                                        else
                                            tempi <- Checked.(+) tempi 1u
                                    | { ParsedRegister.IsTemporary = false; ParsedRegister.Name = _, name as id } ->
                                        match locals.TryGetValue name with
                                        | true, i ->
                                            lmapping.Add(TemporaryIndex.Index tempi, LocalIndex.Index(uint32 i))
                                            tempi <- Checked.(+) tempi 1u
                                        | false, _ ->
                                            registerNotFound id
                                            success <- false)
                                    results

                                index <- Checked.(+) index 1)
                            body

                        if success then
                            { CodeBlock.Locals = lmapping.ToImmutable()
                              Instructions = instrs.ToImmutable() }
                            |> ValueSome
                        else ValueNone

                let resolveCodeBlocks blocks locals arguments =
                    let rec resolveBlockNames blocks (lookup: Dictionary<Name, _>) success =
                        match blocks with
                        | [] when success -> ValueSome lookup
                        | [] -> ValueNone
                        | { ParsedBlock.Symbol = struct(pos, name); ParsedBlock.Instructions = code } :: remaining ->
                            let success' = lookup.TryAdd(name, struct(uint32 lookup.Count, code))
                            if not success' then
                                addValidationError
                                    (sprintf "A block corresponding to the symbol $%O already exists" name)
                                    pos
                            resolveBlockNames remaining lookup success'

                    let rec resolveBlockInstructions (blockIndexLookup: Dictionary<Name, struct(uint32 * _)>) =
                        let mutable blocks', success = Array.zeroCreate<CodeBlock> blockIndexLookup.Count, true

                        for KeyValue(_, struct(i, code)) in blockIndexLookup do
                            match resolveCodeInstructions code i blockIndexLookup locals arguments with
                            | ValueSome block -> blocks'.[Checked.int32 i] <- block
                            | ValueNone -> success <- false

                        if success
                        then ValueSome(Unsafe.As<CodeBlock[], ImmutableArray<CodeBlock>> &blocks')
                        else ValueNone

                    voptional {
                        let! blockIndexLookup = resolveBlockNames blocks (Dictionary()) true
                        return! resolveBlockInstructions blockIndexLookup
                    }

                let addInstructionErrors() =
                    for err in ierrors do
                        match err with
                        | InvalidInstructionError.UndefinedRegister(pos, name) ->
                            addValidationError
                                (sprintf "A register corresponding to the symbol $%O or %%%O could not be found" name name)
                                pos
                        | InvalidInstructionError.InvalidIntegerLiteral(pos, size, literal) ->
                            addValidationError (sprintf "\"%s\" is not a valid %i-bit integer literal" literal size) pos
                        | InvalidInstructionError.UnknownInstruction(pos, name) ->
                            addValidationError (sprintf "%s is not a valid instruction" name) pos
                        | InvalidInstructionError.UndefinedField(pos, name) ->
                            addValidationError (undefinedSymbolMessage "A field" name) pos
                        | InvalidInstructionError.UndefinedMethod(pos, name) ->
                            addValidationError (undefinedSymbolMessage "A method" name) pos
                        | InvalidInstructionError.UndefinedTypeSignature(pos, name) ->
                            addValidationError (undefinedSymbolMessage "A type signature" name) pos
                        | InvalidInstructionError.UndefinedBlock(pos, name) ->
                            addValidationError (undefinedSymbolMessage "A code block" name) pos
                        | InvalidInstructionError.UndefinedData(pos, name) ->
                            addValidationError (undefinedSymbolMessage "A data" name) pos

                mapLookupValues codes <| fun _ code -> voptional {
                    ierrors.Clear()
                    let! locals' = resolveLocalRegisters code.Locals
                    let! arguments' = resolveArgumentRegisters code.Arguments
                    let blocks = resolveCodeBlocks code.Blocks locals' arguments'
                    addInstructionErrors()
                    let! blocks' = blocks
                    return { Code.LocalCount = uint32 locals'.Count; Blocks = blocks' }
                }

            let main' = ValueOption.map (findLookupValue mdefinitions (undefinedSymbolMessage "An entrypoint method")) mmain

            let resolveTypeDefinitions () =
                match tdefinitions' with
                | ValueSome definitions ->
                    let fstart, mstart = uint32 fimports.Count, uint32 mimports.Count
                    let mutable definitions', success = Array.zeroCreate definitions.Length, true

                    for i = 0 to definitions'.Length - 1 do
                        match definitions.[i] fstart mstart with
                        | ValueSome tdef -> definitions'.[i] <- tdef
                        | ValueNone -> success <- false

                    if success
                    then ValueSome(Unsafe.As<TypeDefinition[], ImmutableArray<TypeDefinition>> &definitions')
                    else ValueNone
                | ValueNone -> ValueNone

            let result = voptional {
                let! mname' = mname
                let! namespaces' = namespaces'
                let! tsignatures' = tsignatures'
                let! msignatures' = msignatures'
                let! mimports' = mimports'
                let! timports' = timports'
                let! tdefinitions' = resolveTypeDefinitions()
                let! codes' = codes'
                let! main' =
                    match main' with
                    | ValueNone -> ValueSome ValueNone
                    | ValueSome ValueNone -> ValueNone
                    | ValueSome main'' -> ValueSome main''
                return
                    { Module.Magic = magic
                      FormatVersion = ValueOption.defaultValue currentFormatVersion fversion
                      Header =
                        { ModuleHeader.Module =
                            { ModuleIdentifier.ModuleName = mname'
                              Version = ValueOption.defaultValue VersionNumbers.empty mversion }
                          // TODO: Allow setting of module header flags.
                          Flags = Unchecked.defaultof<ModuleHeaderFlags>
                          PointerSize = PointerSize.Unspecified }
                      Identifiers = { IdentifierSection.Identifiers = identifiers.ToVector() }
                      Namespaces = namespaces'
                      TypeSignatures = tsignatures'
                      MethodSignatures = msignatures'
                      Imports =
                        { ModuleImports.ImportedModules = mimports'
                          ImportedTypes = timports'
                          ImportedFields = fimports.ToVector()
                          ImportedMethods = mimports.ToVector() }
                      Definitions =
                        let adjust index = TypeDefinitionIndex.Index(Checked.(+) (uint32 timports.Count) index)
                        let adjustDefinedMembers (definitions: SymbolDictionary<IndexKinds.Kind, _ -> 'Member>) =
                            let members = definitions.ToVector()
                            let mutable members' = Array.zeroCreate definitions.Count
                            for i = 0 to members'.Length - 1 do members'.[i] <- members.[i] adjust
                            Unsafe.As<'Member[], ImmutableArray<'Member>> &members'
                        { ModuleDefinitions.DefinedTypes = tdefinitions'
                          DefinedFields = adjustDefinedMembers fdefinitions
                          DefinedMethods = adjustDefinedMembers mdefinitions }
                      // TODO: Implement generation of data
                      Data = data.ToVector()
                      Code = codes'
                      EntryPoint = ValueOption.map (fun (Index i) -> Index(Checked.(+) (uint32 mimports.Count) i)) main'
                      Debug = () }
            }

            match result with
            | ValueSome result' -> Result.Ok result'
            | ValueNone -> Result.Error(errors.ToImmutable())
        | decl :: remaining ->
            match decl with
            | ParsedDeclaration.FormatVersion(pos, vnumbers) ->
                match fversion with
                | ValueNone -> fversion <- ValueSome vnumbers
                | ValueSome _ -> addValidationError "Duplicate format version declaration" pos
            | ParsedDeclaration.ModuleVersion(pos, vnumbers) ->
                match mversion with
                | ValueNone -> mversion <- ValueSome vnumbers
                | ValueSome _ -> addValidationError "Duplicate module version declaration" pos
            | ParsedDeclaration.Module(pos, id, name) ->
                match mname with
                | ValueNone ->
                    mname <- ValueSome name
                    //.Add id
                | ValueSome _ -> addValidationError "Duplicate module declaration" pos
            | ParsedDeclaration.Identifier(id, str) ->
                addLookupValue identifiers id str (duplicateSymbolMessage "An identifier")
            | ParsedDeclaration.Signature(id, ParsedSignature.Type signature) ->
                addLookupValue tsignatures id signature (duplicateSymbolMessage "A type signature")
            | ParsedDeclaration.Signature(id, ParsedSignature.Method(rtypes, ptypes)) ->
                addLookupValue msignatures id (rtypes, ptypes) (duplicateSymbolMessage "A method signature")
            | ParsedDeclaration.ImportedModule(id, decls) ->
                addLookupValue mdimports id decls (duplicateSymbolMessage "A module import")
            | ParsedDeclaration.Code(id, code) ->
                addLookupValue codes id code (duplicateSymbolMessage "A method body")
            | ParsedDeclaration.Namespace(id, ns) ->
                addLookupValue namespaces id ns.NamespaceName (duplicateSymbolMessage "A namespace")
            | ParsedDeclaration.TypeDefinition(id, attrs, decls) ->
                addLookupValue tdefinitions id (attrs, decls) (duplicateSymbolMessage "A type definition")
            | ParsedDeclaration.ImportedTypeDefinition(id, decls) ->
                addLookupValue timports id decls (duplicateSymbolMessage "A type definition import")
            | ParsedDeclaration.EntryPoint((pos, _) as main) ->
                match mmain with
                | ValueNone -> mmain <- ValueSome main
                | ValueSome _ -> addValidationError "Duplicate method entry point declaration" pos
            | ParsedDeclaration.Data(id, bytes) ->
                addLookupValue data id (bytes.ToImmutableArray()) (duplicateSymbolMessage "A data")

            inner remaining

    inner declarations

let fromInput input =
    match input Parser.declarations with
    | Success(declarations, (), _) -> assemble declarations
    | Failure(msg, err, ()) -> Result.Error(ImmutableArray.Create(ParserError(msg, err)))
