module UByte.Assembler.Assemble

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open UByte.Helpers

open FParsec

open UByte.Format.Model

open UByte.Assembler.Parser

let emptyIdentifierIndex = IdentifierIndex.Index 0u
let emptyNamespaceIndex = NamespaceIndex.Index 0u

type AssemblerError =
    | ParserError of string * ParserError
    | ValidationError of string * Position

    override this.ToString() =
        match this with
        | ParserError(msg, _)
        | ValidationError(msg, _) -> msg

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

let assemble declarations =
    let errors = ImmutableArray.CreateBuilder<AssemblerError>()
    let mutable fversion, mname, mversion, mmain = ValueNone, ValueNone, ValueNone, ValueNone
    let identifiers = SymbolDictionary<IndexKinds.Identifier, string>()
    do identifiers.AddAnonymous String.Empty |> ignore
    let tsignatures = SymbolDictionary<IndexKinds.TypeSignature, ParsedTypeSignature>()
    let msignatures = SymbolDictionary<IndexKinds.MethodSignature, _>()
    //mimports
    let timports = SymbolDictionary<IndexKinds.Kind, _>()
    let tdefinitions = SymbolDictionary<IndexKinds.Kind, _>()

    let codes = SymbolDictionary<IndexKinds.Code, _>()
    let namespaces = SymbolDictionary<IndexKinds.Namespace, Symbol list>()
    do namespaces.AddAnonymous List.empty |> ignore

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
        | ValueSome _ as i -> i
        | ValueNone -> findLookupValue definitions err symbol

    let rec inner declarations =
        match declarations with
        | [] ->
            if mname.IsNone then
                failwith "Missing module declaration, declare the name of the module with .module MyName"

            // TODO: Resolve all of the things
            let namespaces' = mapLookupValues namespaces (fun _ -> lookupValueList identifiers)

            let inline undefinedSymbolMessage name: Name -> _ =
                sprintf "%s corresponding to the symbol @%O could not be found" name

            let undefinedTypeSignatureMessage = undefinedSymbolMessage "A type definition"

            let lookupID = findLookupValue identifiers (undefinedSymbolMessage "An identifier")

            let lookupDefinedType symbol = // TODO: Use lookupDefinitionOrImport
                //match timports.Find
                voptional {
                    let! (Index i) =
                        findLookupValue
                            tdefinitions
                            undefinedTypeSignatureMessage
                            symbol
                    return TypeDefinitionIndex.Index i
                }

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

            let fimports = SymbolDictionary<IndexKinds.Field, _>()

            let mimports = SymbolDictionary<IndexKinds.Method, _>()

            let timports' = ValueSome ImmutableArray.Empty

            let inline duplicateVisibilityFlag desc pos = addValidationError ("Duplicate visibility flag in " + desc) pos

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
                            match findLookupValue tsignatures undefinedTypeSignatureMessage id with
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
                        return
                            { Field.FieldOwner = owner
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
                            match body' methodBodyLookup with
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
                                resolveMethodDeclarations true visibility flags name signature' body remaining
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
                        return
                            { Method.MethodOwner = owner
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

            let fdefinitions = SymbolDictionary<IndexKinds.Field, _>()
            let mdefinitions = SymbolDictionary<IndexKinds.Method, _>()

            let tdefinitions' =
                let fields = ImmutableArray.CreateBuilder()
                let methods = ImmutableArray.CreateBuilder()

                let rec resolveTypeAttributes attributes =
                    let rec inner success hasvis visibility attributes =
                        match attributes with
                        | [] when success -> ValueSome visibility
                        | [] -> ValueNone
                        | TypeDefAttr.Visibility(pos, flag) :: remaining ->
                            if not hasvis then
                                inner success true flag remaining
                            else
                                duplicateVisibilityFlag "type definition" pos
                                inner false true visibility remaining
                    inner true false VisibilityFlags.Unspecified attributes

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
                                    | ValueSome i ->
                                        fields.Add i
                                        true
                                    | ValueNone ->
                                        false
                                inner success' ns name remaining
                            | ValueNone ->
                                inner false ns name remaining
                        | TypeDefDecl.Method(id, mattrs, mdecls) :: remaining ->
                            match resolveMethodDefinition owner mattrs mdecls with
                            | ValueSome field ->
                                let success' =
                                    match mdefinitions.Add(id, field) with
                                    | ValueSome i ->
                                        methods.Add i
                                        true
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

                    inner true ValueNone ValueNone declarations

                mapLookupValues tdefinitions <| fun (Index owner) (attrs, decls) -> voptional {
                    fields.Clear()
                    methods.Clear()
                    let! visibility = resolveTypeAttributes attrs
                    let! (ns, name) = resolveTypeDeclarations (Index(owner + uint32 timports.Count)) decls
                    return
                        { TypeDefinition.TypeName = ValueOption.defaultValue emptyIdentifierIndex name
                          TypeNamespace = ValueOption.defaultValue emptyNamespaceIndex ns
                          TypeVisibility = visibility
                          TypeKind = TypeDefinitionKind.Struct
                          TypeLayout = TypeDefinitionLayout.Unspecified
                          ImplementedInterfaces = ImmutableArray.Empty
                          TypeParameters = ImmutableArray.Empty
                          TypeAnnotations = ImmutableArray.Empty
                          Fields = fields.ToImmutable()
                          Methods = methods.ToImmutable() }
                }

            let codes' =
                let resolveTypeSignature =
                    findLookupValue tsignatures (sprintf "A type signature corresponding to the symbol @%O could not be found")

                let resolveLocalRegisters =
                    let registers = ImmutableArray.CreateBuilder<struct(_ * RegisterType)>()

                    let countLocalRegisters (lookup: Dictionary<_, _>) names =
                        let rec inner names count success =
                            match names with
                            | [] when success -> ValueSome count
                            | [] -> ValueNone
                            | ((_, name): Symbol) :: remaining ->
                                inner
                                    remaining
                                    (Checked.(+) 1u count)
                                    (success && lookup.TryAdd(name, RegisterIndex.Index(uint32 lookup.Count)))
                        inner names 0u true

                    let rec inner locals lookup success =
                        match locals with
                        | [] when success -> ValueSome(registers.ToImmutable(), lookup :> IReadOnlyDictionary<_, _>)
                        | [] -> ValueNone
                        | loc :: remaining ->
                            let result = voptional {
                                let! ltype = resolveTypeSignature loc.LocalsType
                                let! count = countLocalRegisters lookup loc.LocalNames
                                return struct(count, { RegisterType = ltype; RegisterFlags = RegisterFlags.None })
                            }

                            let success' =
                                match result with
                                | ValueSome rt when success ->
                                    registers.Add rt
                                    true
                                | _ ->
                                    false

                            inner remaining lookup success'

                    fun locals ->
                        registers.Clear()
                        inner locals (Dictionary()) true

                let ierrors = List<InvalidInstructionError> 0

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
                            findLookupValue tsignatures (undefinedSymbolMessage "A type signature") symbol }

                let resolveCodeInstructions =
                    let instrs = ImmutableArray.CreateBuilder<InstructionSet.Instruction>()

                    let rec inner instructions rlookup success =
                        match instructions with
                        | [] when success -> ValueSome(instrs.ToImmutable())
                        | [] -> ValueNone
                        | i :: remaining ->
                            let success' =
                                match i rlookup iresolver (ierrors :> InstructionErrorsBuilder) with
                                | ValueSome i' when success ->
                                    instrs.Add(i')
                                    true
                                | _ ->
                                    false

                            inner remaining rlookup success'

                    fun instructions (rlookup: IReadOnlyDictionary<_, _>) ->
                        instrs.Clear()
                        ierrors.Clear()
                        inner
                            instructions
                            (fun ((_, register): Symbol) ->
                                match rlookup.TryGetValue register with
                                | true, i -> ValueSome i
                                | false, _ -> ValueNone)
                            true

                let addInstructionErrors() =
                    for err in ierrors do
                        match err with
                        | InvalidInstructionError.UndefinedRegister(pos, name) ->
                            addValidationError (sprintf "A register corresponding to the symbol @%O could not be found" name) pos
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

                mapLookupValues codes <| fun _ code -> voptional {
                    let! (registers, rlookup) = resolveLocalRegisters code.Locals
                    let instructions = resolveCodeInstructions code.Instructions rlookup
                    addInstructionErrors()
                    let! instructions' = instructions
                    return { Code.RegisterTypes = registers; Instructions = instructions' }
                }

            let main' = ValueOption.map (findLookupValue mdefinitions (undefinedSymbolMessage "An entrypoint method")) mmain

            let result = voptional {
                let! mname' = mname
                let! namespaces' = namespaces'
                let! tsignatures' = tsignatures'
                let! msignatures' = msignatures'
                //let! mimports' = mimports'
                let! timports' = timports'
                let! tdefinitions' = tdefinitions'
                let! codes' = codes'
                let! main' = main'
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
                        { ModuleImports.ImportedModules = ImmutableArray.Empty // TODO: Implement generation of module imports
                          ImportedTypes = timports'
                          ImportedFields = fimports.ToVector()
                          ImportedMethods = mimports.ToVector() }
                      Definitions =
                        { ModuleDefinitions.DefinedTypes = tdefinitions'
                          DefinedFields = fdefinitions.ToVector()
                          DefinedMethods = mdefinitions.ToVector() }
                      // TODO: Implement generation of data
                      Data = ImmutableArray.Empty
                      Code = codes'
                      EntryPoint = main'
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
            //| ParsedDeclaration.ModuleImport
            | ParsedDeclaration.Code(id, code) ->
                addLookupValue codes id code (duplicateSymbolMessage "A method body")
            | ParsedDeclaration.Namespace(id, ns) ->
                addLookupValue namespaces id ns.NamespaceName (duplicateSymbolMessage "A namespace")
            | ParsedDeclaration.TypeDefinition(id, attrs, decls) ->
                addLookupValue tdefinitions id (attrs, decls) (duplicateSymbolMessage "A type definition")
            | ParsedDeclaration.EntryPoint((pos, _) as main) ->
                match mmain with
                | ValueNone -> mmain <- ValueSome main
                | ValueSome _ -> addValidationError "Duplicate method entry point declaration" pos
            | ParsedDeclaration.ImportedModule _ -> failwith "TODO: Module imports not yet supported"

            inner remaining

    inner declarations

let fromInput input =
    match input Parser.declarations with
    | Success(declarations, (), _) -> assemble declarations
    | Failure(msg, err, ()) -> Result.Error(ImmutableArray.Create(ParserError(msg, err)))
