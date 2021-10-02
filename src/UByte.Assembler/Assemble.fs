module UByte.Assembler.Assemble

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FParsec

open UByte.Format.Model

// TODO: Use |>> and FParsec to make assembler easier

let emptyIdentifierIndex = IdentifierIndex.Index 0u

type AssemblerError =
    | ParserError of string * ParserError
    | ValidationError of string * Position

    override this.ToString() =
        match this with
        | ParserError(msg, _)
        | ValidationError(msg, _) -> msg

[<Sealed>]
type SymbolDictionary<'IndexKind, 'T when 'IndexKind :> IndexKinds.Kind> () = // TODO: Rename to Identifier lookup
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
            let (Index i) = this.AddAnonymous item
            symbols.Add(id, Checked.int32 i)
        not exists

    member this.Add(id, item) =
        match id with
        | ValueSome symbol -> this.AddNamed(symbol, item)
        | ValueNone -> this.AddAnonymous item |> ignore; true

    member _.FromIdentifier id: Index<'IndexKind> voption =
        match symbols.TryGetValue id with
        | true, item -> ValueSome(Index(Checked.uint32 item))
        | false, _ -> ValueNone

    member _.ToVector(): vector<'T> = items.ToImmutable()

    member _.ValueAt i = items.[i]

type IdentifierLookup = SymbolDictionary<IndexKinds.Identifier, string>

type TypeSignatureLookup =
    SymbolDictionary<IndexKinds.TypeSignature, (Name -> TypeDefinitionIndex voption) -> Result<AnyType, AssemblerError>>

[<Sealed>]
type ModuleImportLookup () =
    let imports = SymbolDictionary<IndexKinds.Module, ModuleIdentifier>()

    member _.Add(id, import) = imports.Add(id, import)
    member _.Find id: ModuleIndex voption =
        match imports.FromIdentifier id with
        | ValueSome(Index i) -> ValueSome(Index(i + 1u))
        | ValueNone -> ValueNone

    member _.Imports = imports.Values

[<NoComparison; NoEquality>]
type UnresolvedTypeDefinitionImport =
    { TypeOwner: Position * Name
      TypeName: Position * Name
      TypeNamespace: Position * Name
      IsStruct: bool }

[<NoComparison; NoEquality>]
type UnresolvedTypeDefinition =
    { TypeName: Position * Name
      TypeNamespace: Position * Name
      TypeVisibility: VisibilityFlags
      TypeKind: (Name -> TypeDefinitionIndex voption) -> Result<TypeDefinitionKind, AssemblerError> }

// TODO: Use these to replace TypeDefinitionLookup, MethodLookup, and FieldLookup
type SharedIdentifierLookup<'IndexKind, 'Imported, 'Defined when 'IndexKind :> IndexKinds.Kind> () =
    let imported = SymbolDictionary<'IndexKind, 'Imported>()
    let defined = SymbolDictionary<'IndexKind, 'Defined>()

    member _.Definitions = defined.Values
    member _.Imports = imported.Values

    member _.AddDefinition(name, definition) = defined.Add(name, definition)
    /// Be sure to only lookup definitions once all imports have been added to avoid changing of indices.
    member _.FindDefinition name: Index<'IndexKind> voption =
        match defined.FromIdentifier name with
        | ValueSome(Index i) -> ValueSome(Index(i + uint32 imported.Count))
        | ValueNone -> ValueNone

    member _.AddImport(name, import) = imported.Add(name, import)
    member _.FindImport name = imported.FromIdentifier name

    member this.Find name: Index<'IndexKind> voption =
        match this.FindImport name with
        | ValueSome _ as i -> i
        | ValueNone -> this.FindDefinition name

type TypeDefinitionLookup = SharedIdentifierLookup<IndexKinds.TypeDefinition, UnresolvedTypeDefinitionImport, UnresolvedTypeDefinition>

[<NoComparison; NoEquality>]
type UnresolvedField =
    { FieldOwner: Position * Name
      FieldName: Position * Name
      FieldVisibility: VisibilityFlags
      FieldFlags: FieldFlags
      FieldType: Position * Name }

[<Sealed>]
type FieldLookup () =
    let defined = SymbolDictionary<IndexKinds.Field, UnresolvedField>() // TODO: Use SharedIdentifierLookup instead.

    member _.Definitions = defined.Values
    member _.AddDefinition(name, item) = defined.Add(name, item)
    member _.FindDefinition name = defined.FromIdentifier name
    member this.Find name = this.FindDefinition name

[<NoComparison; NoEquality>]
type UnresolvedMethodImport =
    { MethodOwner: Position * Name
      MethodName: Position * Name
      MethodSignature: Position * Name }

[<NoComparison; NoEquality>]
type UnresolvedMethod =
    { MethodOwner: Position * Name
      MethodName: (Position * Name) voption
      MethodVisibility: VisibilityFlags
      MethodFlags: MethodFlags
      MethodSignature: Position * Name
      MethodBody: (Name -> CodeIndex voption) -> Result<MethodBody, AssemblerError> }

type MethodLookup = SharedIdentifierLookup<IndexKinds.Method, UnresolvedMethodImport, UnresolvedMethod>

[<NoComparison; NoEquality; IsReadOnly; Struct>]
type UnresolvedMethodSignature = { ReturnTypes: (Position * Name) list; ParameterTypes: (Position * Name) list }

type UnresolvedInstruction =
    (Name -> RegisterIndex voption) -> FieldLookup -> MethodLookup -> Result<InstructionSet.Instruction, AssemblerError list>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type UnresolvedRegisterType =
    | Local of Position * Name
    | Argument of RegisterIndex

[<NoComparison; NoEquality; IsReadOnly; Struct>]
type UnresolvedCodeRegister = { RegisterName: Position * Name; RegisterType: UnresolvedRegisterType }

[<NoComparison; NoEquality; IsReadOnly; Struct>]
type UnresolvedCode = { Registers: UnresolvedCodeRegister list; Instructions: UnresolvedInstruction list }

[<NoComparison; NoEquality>]
type IncompleteModule =
    { ModuleFormatVersion: VersionNumbers voption ref
      mutable ModuleHeaderName: Name voption
      ModuleHeaderVersion: VersionNumbers voption ref
      ModuleIdentifiers: IdentifierLookup
      ModuleNamespaces: SymbolDictionary<IndexKinds.Namespace, (Position * Name) list>
      ModuleTypeSignatures: TypeSignatureLookup
      ModuleMethodSignatures: SymbolDictionary<IndexKinds.MethodSignature, UnresolvedMethodSignature>
      ModuleImports: ModuleImportLookup
      ModuleTypes: TypeDefinitionLookup
      ModuleFields: FieldLookup
      ModuleMethods: MethodLookup
      ModuleCode: SymbolDictionary<IndexKinds.Code, UnresolvedCode>
      mutable ModuleEntryPoint: (Position * Name) voption }

[<Struct>]
type State = { Module: IncompleteModule; Errors: AssemblerError list }

let errorBadIdentifier name reason (id: Name) pos =
    ValidationError("A " + name + " corresponding to the identifier $" + string id + " " + reason, pos)

let errorDuplicateIdentifier name id pos = errorBadIdentifier name "already exists" id pos
let errorIdentifierUndefined name id pos = errorBadIdentifier name "was not defined" id pos

let whitespace =
    spaces
    .>> optional (attempt (skipChar ';') .>> skipRestOfLine true <?> "single-line comment")
    .>> spaces

let atom p = whitespace >>. p .>> whitespace
let atomL name p = atom p <?> name

let idchar = choice [ asciiLetter; digit; anyOf "_.<>";  ]
let identifier = skipChar '$' >>. (many1Chars idchar) |>> Name.ofStr |> atomL "identifier"
let stringlit = let quot = skipChar '\"' in quot >>. manyCharsTill idchar quot |> atomL "string literal"
let integerlit = atomL "integer literal" pint64
let keyword = skipString >> atom

/// <summary>Parses text nested within parenthesis.</summary>
/// <remarks>
/// Do not use <c>choice</c> as the <paramref name="inner"/> parser if the choice parsers also parse s-expressions, as the first
/// parser will consume the opening parenthesis.
/// </remarks>
let sexpression inner = between (skipChar '(') (skipChar ')') inner |> atom

let declaration name inner =
    keyword name
    >>? inner
    |> sexpression
    |> atom

let inline attributes (flags: (string * 'Flag) list when 'Flag :> Enum) =
    List.map (fun (name, flag) -> keyword name >>. preturn flag) flags
    |> choice
    |> many
    |>> List.fold (fun flags flag -> flags ||| flag) Unchecked.defaultof<_>

let inline flags name flags =
    choice [
        declaration name (attributes flags) |> attempt
        preturn Unchecked.defaultof<_>
    ]

type AssemblerResult = Result<Module, AssemblerError list>

[<Sealed>]
type AssemblerResultBuilder () =
    member inline _.Bind(result: Result<_, AssemblerError list>, body: 'T -> Result<'U, _>) =
        match result with
        | Result.Ok result' -> body result'
        | Result.Error err -> Result.Error err

    member inline _.Return value = Result<_, AssemblerError list>.Ok value

    member inline _.ReturnFrom(result: Result<_, AssemblerError list>) = result

let validated = AssemblerResultBuilder()

let vnumbers = (many integerlit |>> (List.map uint32 >> ImmutableArray.CreateRange >> VersionNumbers))

let tidentifier =
    keyword "identifier" >>. tuple3 getPosition identifier stringlit >>= fun (pos, id, name) ->
        updateUserState <| fun state ->
            if state.Module.ModuleIdentifiers.AddNamed(id, name) then
                state
            else
                { state with Errors = errorDuplicateIdentifier "identifier" id pos :: state.Errors }

let ttypesig =
    keyword "type" >>. choice [
        choiceL
            [
                skipString "s32" >>. preturn PrimitiveType.S32
            ]
            "primitive type"
        |>> fun prim -> fun _ -> Result.Ok(AnyType.Primitive prim)

        choice [
            keyword "ref" >>. choice [
                keyword "any" >>. preturn (fun _ -> Result.Ok ReferenceType.Any)

                keyword "def" >>. getPosition .>>. identifier |>> fun (pos, id) -> fun lookup ->
                    match lookup id with
                    | ValueSome tindex ->
                        Result.Ok(ReferenceType.Defined tindex)
                    | ValueNone ->
                        Result.Error(errorIdentifierUndefined "type definition" id pos)
            ]
            <?> "reference type"
            |>> fun o -> fun lookup -> Result.map AnyType.ObjectReference (o lookup)
        ]
        |> sexpression
    ]

let tmethodsig =
    let typelist name =
        many (getPosition .>>. identifier)
        |> declaration name
        |> attempt // TODO: Parse an open paren then check if keyword is returns or parameters.
        |> opt
        |>> Option.defaultValue List.empty

    keyword "method" >>. pipe2 (typelist "returns") (typelist "parameters") (fun returnt paramt ->
        { UnresolvedMethodSignature.ReturnTypes = returnt; ParameterTypes = paramt })

let tsignature =
    tuple3
        (keyword "signature" >>. getPosition)
        identifier
        (choice [ ttypesig |>> Choice1Of2; tmethodsig |>> Choice2Of2 ] |> sexpression)
    >>= fun(pos, id, signature) ->
        updateUserState <| fun state ->
            match signature with
            | Choice1Of2 t ->
                if state.Module.ModuleTypeSignatures.AddNamed(id, t) then
                    state
                else
                    { state with Errors = errorDuplicateIdentifier "type signature" id pos :: state.Errors }
            | Choice2Of2 m ->
                if state.Module.ModuleMethodSignatures.AddNamed(id, m) then
                    state
                else
                    { state with Errors = errorDuplicateIdentifier "method signature" id pos :: state.Errors }

let tcode =
    let registers =
        choice [
            keyword "local" >>. (getPosition .>>. identifier) .>>. (getPosition .>>. identifier) |>> fun (rname, rtype) ->
                { UnresolvedCodeRegister.RegisterName = rname
                  RegisterType = UnresolvedRegisterType.Local rtype }

            keyword "argument" >>. (getPosition .>>. identifier) .>>. integerlit |>> fun (rname, rindex) ->
                { UnresolvedCodeRegister.RegisterName = rname
                  RegisterType = UnresolvedRegisterType.Argument(Index(Checked.uint32 rindex)) }
        ]
        |> sexpression
        |> many
        |> declaration "registers"
        // TODO: Registers should be optional

    let lookupRegisterName lookup (pos, name) =
        match lookup name with
        | ValueSome i -> Result.Ok i
        | ValueNone -> Result.Error(errorIdentifierUndefined "register" name pos)

    let lookupRegisterList lookup names =
        let rec inner (registers: ImmutableArray<RegisterIndex>.Builder) errors names =
            match errors, names with
            | _ :: _, [] -> Result.Error errors
            | [], [] -> Result.Ok(registers.ToImmutable())
            | _, name :: remaining ->
                let errors' =
                    match lookupRegisterName lookup name with
                    | Result.Ok i ->
                        registers.Add i
                        errors
                    | Result.Error err ->
                        err :: errors
                inner registers errors' remaining
        inner (ImmutableArray.CreateBuilder()) List.empty names

    let lookupRegisterArray lookup names =
        let mutable resolved, errors = Array.zeroCreate(Array.length names), List.empty

        for i = 0 to resolved.Length - 1 do
            match lookupRegisterName lookup names.[i] with
            | Result.Ok rindex -> resolved.[i] <- rindex
            | Result.Error err -> errors <- err :: errors

        match errors with
        | [] -> Result.Ok(Unsafe.As<RegisterIndex[], ImmutableArray<RegisterIndex>> &resolved)
        | _ -> Result.Error errors

    let lookupMethodName (mlookup: MethodLookup) (pos, name) =
        match mlookup.Find name with
        | ValueSome mindex -> Result.Ok mindex
        | ValueNone -> Result.Error [ errorIdentifierUndefined "method" name pos ]

    let body: Parser<UnresolvedInstruction list, _> =
        choice [
            choice [
                keyword "ret" >>. many (getPosition .>>. identifier) |>> fun names -> fun registers _ _ ->
                    Result.map InstructionSet.Ret (lookupRegisterList registers names)

                keyword "add" >>. parray 3 (getPosition .>>. identifier) |>> fun names -> fun registers _ _ ->
                    Result.map
                        (fun (registers': ImmutableArray<_>) ->
                            InstructionSet.Add(registers'.[0], registers'.[1], registers'.[2]))
                        (lookupRegisterArray registers names)

                let inline guardIntegerRange min max convert value =
                    if value >= int64 min && value <= int64 max
                    then ValueSome(convert value)
                    else ValueNone

                keyword "const.i32"
                >>. (getPosition .>>. integerlit)
                .>>. (getPosition .>>. identifier)
                |>> fun (value, name) -> fun registers _ _ ->
                    match guardIntegerRange Int32.MinValue Int32.MaxValue int32 (snd value) with
                    | ValueSome value' ->
                        lookupRegisterName registers name
                        |> Result.map (fun i -> InstructionSet.Const_i32(value', i))
                        |> Result.mapError List.singleton
                    | ValueNone ->
                        Result.Error [ ValidationError(string (snd value) + " is not a valid 32-bit integer literal", fst value) ]

                keyword "obj.null" >>. getPosition .>>. identifier |>> fun r -> fun registers _ _ ->
                    lookupRegisterName registers r
                    |> Result.map InstructionSet.Obj_null
                    |> Result.mapError List.singleton

                keyword "obj.new" >>. tuple3
                    (getPosition .>>. identifier)
                    (declaration "arguments" (many (getPosition .>>. identifier)))
                    (declaration "returns" (getPosition .>>. identifier))
                |>> fun (mname, args, ret) -> fun registers _ mlookup ->
                    validated {
                        let! args' = lookupRegisterList registers args
                        let! ret' = lookupRegisterName registers ret |> Result.mapError List.singleton
                        let! constructor = lookupMethodName mlookup mname
                        return InstructionSet.Obj_new(constructor, args', ret')
                    }

                let objectFieldInstruction name instr =
                    keyword name >>. tuple3
                        (getPosition .>>. identifier)
                        (getPosition .>>. identifier)
                        (getPosition .>>. identifier)
                    |>> fun (field, oreg, reg) -> fun registers (flookup: FieldLookup) _ ->
                        validated {
                            let! field' =
                                match flookup.Find(snd field) with
                                | ValueSome findex -> Result.Ok findex
                                | ValueNone -> Result.Error [ errorIdentifierUndefined "field" (snd field) (fst field) ]

                            let! oreg' = lookupRegisterName registers oreg |> Result.mapError List.singleton
                            let! reg' = lookupRegisterName registers reg |> Result.mapError List.singleton

                            return instr(field', oreg', reg')
                        }

                objectFieldInstruction "obj.ldfd" InstructionSet.Obj_ldfd
                objectFieldInstruction "obj.stfd" InstructionSet.Obj_stfd

                let callLikeInstruction =
                    let registers = many (getPosition .>>. identifier)

                    fun name instr ->
                        keyword name >>. tuple3
                            (getPosition .>>. identifier)
                            (declaration "arguments" registers)
                            (declaration "returns" registers)
                        |>> fun (mname, args, rets) -> fun registers _ mlookup ->
                            validated {
                                let! args' = lookupRegisterList registers args
                                let! rets' = lookupRegisterList registers rets
                                let! method = lookupMethodName mlookup mname
                                return instr(method, args', rets')
                            }

                callLikeInstruction "call" InstructionSet.Call
                callLikeInstruction "call.virt" InstructionSet.Call_virt
                callLikeInstruction "call.ret" InstructionSet.Call_ret
                callLikeInstruction "call.virt.ret" InstructionSet.Call_virt_ret
            ]
            |> sexpression

            let noOperandInstruction name instr =
                keyword name >>. preturn (fun _ _ _ -> Result.Ok instr)

            noOperandInstruction "ret" (InstructionSet.Ret ImmutableArray.Empty)
            noOperandInstruction "nop" InstructionSet.Nop
        ]
        |> many

    keyword "code" >>. tuple4 getPosition identifier registers body >>= fun (pos, id, regs, instrs) ->
        updateUserState <| fun state ->
            if state.Module.ModuleCode.AddNamed(id, { UnresolvedCode.Registers = regs; Instructions = instrs }) then
                state
            else
                { state with Errors = errorDuplicateIdentifier "code block" id pos :: state.Errors }

let tnamespace =
    tuple3
        (keyword "namespace" >>. getPosition)
        identifier
        (declaration "name" (many (getPosition .>>. identifier)))
    >>= fun (pos, id, name) ->
        updateUserState <| fun state ->
            if state.Module.ModuleNamespaces.AddNamed(id, name) then
                state
            else
                { state with Errors = errorDuplicateIdentifier "method signature" id pos :: state.Errors }

let tnamedecl = getPosition .>>. declaration "name" identifier
let tnamespacedecl = getPosition .>>. declaration "namespace" identifier

let tvisibility =
    flags "visibility" [
        "public", VisibilityFlags.Public
        "private", VisibilityFlags.Private
    ]

let timports: Parser<unit, _> =
    let inner =
        choice [
            keyword "module" >>. tuple3
                (getPosition .>>. identifier)
                (declaration "name" stringlit)
                (declaration "version" vnumbers)
            >>= fun ((pos, id), name, version) ->
                updateUserState <| fun state ->
                    let import =
                        { ModuleIdentifier.ModuleName = Name.ofStr name
                          Version = version }

                    if state.Module.ModuleImports.Add(ValueSome id, import) then
                        state
                    else
                        { state with Errors = errorDuplicateIdentifier "module imports" id pos :: state.Errors }

            keyword "type" >>. tuple5
                (getPosition .>>. identifier)
                (declaration "module" (getPosition .>>. identifier))
                tnamedecl
                tnamespacedecl
                (choice [ keyword "class" >>. preturn true; keyword "struct" >>. preturn false ])
            >>= fun ((pos, id), owner, name, ns, isValueType) ->
                updateUserState <| fun state ->
                    let t =
                        { UnresolvedTypeDefinitionImport.TypeOwner = owner
                          TypeName = name
                          TypeNamespace = ns
                          IsStruct = isValueType }

                    if state.Module.ModuleTypes.AddImport(ValueSome id, t) then
                        state
                    else
                        { state with Errors = errorDuplicateIdentifier "type definition import" id pos :: state.Errors }

            keyword "method" >>. tuple4
                (getPosition .>>. identifier)
                tnamedecl
                (declaration "signature" (getPosition .>>. identifier))
                (declaration "owner" (getPosition .>>. identifier))
            >>= fun ((pos, id), name, signature, owner) ->
                updateUserState <| fun state ->
                    let m =
                        { UnresolvedMethodImport.MethodOwner = owner
                          MethodName = name
                          MethodSignature = signature }

                    if state.Module.ModuleMethods.AddImport(ValueSome id, m) then
                        state
                    else
                        { state with Errors = errorDuplicateIdentifier "module import" id pos :: state.Errors }
        ]
        |> sexpression
        |> skipMany

    keyword "imports" .>> inner

let ttypedef =
    let kind: Parser<_, _> = choice [
        choice [
            keyword "base" >>. preturn (fun _ -> ValueNone)

            //declaration "extends"
        ]
        .>>. attributes List.empty //[ "abstract", ClassDefinitionFlags.Abstract ]
        |> declaration "class"
        |>> fun (extends, flags) -> fun lookup -> TypeDefinitionKind.Class(extends lookup, flags) |> Result.Ok
    ]

    tuple5
        (keyword "type" >>. getPosition .>>. identifier)
        tnamedecl
        tnamespacedecl
        tvisibility
        kind
    >>= fun ((pos, id), name, ns, vis, tkind) ->
        updateUserState <| fun state ->
            let tdef =
                { UnresolvedTypeDefinition.TypeName = name
                  TypeNamespace = ns
                  TypeVisibility = vis
                  TypeKind = tkind }

            if state.Module.ModuleTypes.AddDefinition(ValueSome id, tdef) then
                state
            else
                { state with Errors = errorDuplicateIdentifier "type definition" id pos :: state.Errors }

let tmethodkind: Parser<_, _> = choice [
    getPosition .>>. identifier
    |>> fun (pos, name) -> fun lookup ->
        match lookup name with
        | ValueSome i -> Result.Ok(MethodBody.Defined i)
        | ValueNone -> Result.Error(errorIdentifierUndefined "code block" name pos)
    |> declaration "defined"
]

let tmethoddef =
    let mflags: Parser<_, _> = flags "flags" [
        "instance", MethodFlags.Instance
    ]

    tuple5
        (tuple3 (keyword "method" >>. getPosition) identifier tnamedecl)
        tvisibility
        (getPosition .>>. declaration "signature" identifier)
        (tmethodkind .>>. mflags)
        (getPosition .>>. declaration "owner" identifier)
    >>= fun ((pos, id, name), vis, signature, (mkind, mflags), owner) ->
        updateUserState <| fun state ->
            let mdef =
                { UnresolvedMethod.MethodOwner = owner
                  MethodName = ValueSome name
                  MethodVisibility = vis
                  MethodFlags = mflags
                  MethodSignature = signature
                  MethodBody = mkind }

            if state.Module.ModuleMethods.AddDefinition(ValueSome id, mdef) then
                state
            else
                { state with Errors = errorDuplicateIdentifier "method definition" id pos :: state.Errors }

let tctordef =
    tuple5
        (keyword "constructor" >>. getPosition)
        (identifier .>>. tvisibility)
        (getPosition .>>. declaration "signature" identifier)
        tmethodkind
        (getPosition .>>. declaration "owner" identifier)
    >>= fun (pos, (id, vis), signature, mkind, owner) ->
        updateUserState <| fun state ->
            let mdef =
                { UnresolvedMethod.MethodOwner = owner
                  MethodName = ValueNone
                  MethodVisibility = vis
                  MethodFlags = MethodFlags.ConstructorMask
                  MethodSignature = signature
                  MethodBody = mkind }

            if state.Module.ModuleMethods.AddDefinition(ValueSome id, mdef) then
                state
            else
                { state with Errors = errorDuplicateIdentifier "method definition" id pos :: state.Errors }

let tfielddef =
    let fflags =
        flags "flags" [
            "mutable", FieldFlags.Mutable
            "static", FieldFlags.Static
        ]

    keyword "field" >>. tuple5
        (getPosition .>>. identifier)
        (tnamedecl .>>. tvisibility)
        (getPosition .>>. declaration "type" identifier)
        fflags
        (getPosition .>>. declaration "owner" identifier)
    >>= fun ((pos, id), (name, vis), t, flags, owner) ->
        updateUserState <| fun state ->
            let field =
                { UnresolvedField.FieldOwner = owner
                  FieldName = name
                  FieldVisibility = vis
                  FieldFlags = flags
                  FieldType = t }

            if state.Module.ModuleFields.AddDefinition(ValueSome id, field) then
                state
            else
                { state with Errors = errorDuplicateIdentifier "field definition" id pos :: state.Errors }

let tryMapLookup (lookup: SymbolDictionary<_, 'T>) (mapping: 'T -> Result<'U, AssemblerError list>) =
    let mutable items = Array.zeroCreate lookup.Count
    let rec inner i errors =
        if i < lookup.Count then
            let errors' =
                match mapping(lookup.ValueAt i) with
                | Result.Ok result ->
                    items.[i] <- result
                    errors
                | Result.Error err ->
                    err @ errors
            inner (i + 1) errors'
        else
            match errors with
            | [] -> Result.Ok(Unsafe.As<'U[], ImmutableArray<'U>> &items)
            | _ -> Result.Error errors
    inner 0 List.empty

let buildLookupValues (lookup: SymbolDictionary<_, _>) =
    let builder = ImmutableArray.CreateBuilder()
    let rec inner names errors =
        match names, errors with
        | [], [] -> Result.Ok(builder.ToImmutable())
        | [], _ -> Result.Error errors
        | (pos, name) :: remaining, _ ->
            let errors' =
                match lookup.FromIdentifier name with
                | ValueSome i ->
                    builder.Add i
                    errors
                | ValueNone ->
                    errorIdentifierUndefined "string" name pos :: errors
            inner remaining errors'
    fun names ->
        builder.Clear()
        inner names List.empty

let buildTypeDefinitions
    (identifiers: IdentifierLookup)
    (modules: ModuleImportLookup)
    (namespaces: SymbolDictionary<_, _>)
    (types: TypeDefinitionLookup)
    (methodOwnerLookup: IReadOnlyDictionary<_, ImmutableArray<_>.Builder>)
    (fieldOwnerLookup: IReadOnlyDictionary<_, ImmutableArray<_>.Builder>)
    =
    let imports = ImmutableArray.CreateBuilder types.Imports.Count
    let definitions = ImmutableArray.CreateBuilder types.Definitions.Count
    let mutable errors, i = List.empty, 0u

    for t in types.Imports do
        let result =
            validated {
                let! towner =
                    let (pos, name) = t.TypeOwner
                    match modules.Find name with
                    | ValueSome namei -> Result.Ok namei
                    | ValueNone -> Result.Error [ errorIdentifierUndefined "module import" name pos ]

                let! tname =
                    let (pos, name) = t.TypeName
                    match identifiers.FromIdentifier name with
                    | ValueSome namei -> Result.Ok namei
                    | ValueNone -> Result.Error [ errorIdentifierUndefined "type name" name pos ]

                // TODO: Reduce code duplication with lookups involving Names
                let! tnamespace =
                    let (pos, name) = t.TypeNamespace
                    match namespaces.FromIdentifier name with
                    | ValueSome nsi -> Result.Ok nsi
                    | ValueNone -> Result.Error [ errorIdentifierUndefined "type namespace" name pos ]

                return! Result.Ok
                    { TypeDefinitionImport.Module = towner
                      TypeName = tname
                      TypeNamespace = tnamespace
                      IsStruct = t.IsStruct
                      TypeParameters = 0u }
            }

        match result with
        | Result.Ok t' -> imports.Add t'
        | Result.Error errors' -> errors <- errors' @ errors

        i <- Checked.(+) i 1u

    for t in types.Definitions do
        let result =
            validated {
                let! tname =
                    let (pos, name) = t.TypeName
                    match identifiers.FromIdentifier name with
                    | ValueSome namei -> Result.Ok namei
                    | ValueNone -> Result.Error [ errorIdentifierUndefined "type name" name pos ]

                // TODO: Reduce code duplication with lookups involving Names
                let! tnamespace =
                    let (pos, name) = t.TypeNamespace
                    match namespaces.FromIdentifier name with
                    | ValueSome nsi -> Result.Ok nsi
                    | ValueNone -> Result.Error [ errorIdentifierUndefined "type namespace" name pos ]

                let! tkind = Result.mapError List.singleton (t.TypeKind types.Find)

                let tindex = TypeDefinitionIndex.Index i

                return! Result.Ok
                    { TypeDefinition.TypeName = tname
                      TypeNamespace = tnamespace
                      TypeVisibility = t.TypeVisibility
                      TypeKind = tkind
                      TypeLayout = TypeDefinitionLayout.Unspecified // TODO: Allow setting of type layout
                      ImplementedInterfaces = ImmutableArray.Empty
                      TypeParameters = ImmutableArray.Empty
                      TypeAnnotations = ImmutableArray.Empty
                      Fields =
                        match fieldOwnerLookup.TryGetValue tindex with
                        | true, existing -> existing.ToImmutable()
                        | false, _ -> ImmutableArray.Empty
                      Methods =
                        match methodOwnerLookup.TryGetValue tindex with
                        | true, existing -> existing.ToImmutable()
                        | false, _ -> ImmutableArray.Empty }
            }

        match result with
        | Result.Ok t' -> definitions.Add t'
        | Result.Error errors' -> errors <- errors' @ errors

        i <- Checked.(+) i 1u

    match errors with
    | [] -> Result.Ok(imports.ToImmutable(), definitions.ToImmutable())
    | _ -> Result.Error errors

let buildFieldDefinitions
    (identifiers: IdentifierLookup)
    (signatures: TypeSignatureLookup)
    (types: TypeDefinitionLookup)
    (fields: FieldLookup)
    (fieldOwnerLookup: IDictionary<_, _>)
    =
    let builder = ImmutableArray.CreateBuilder()
    let mutable errors, i = List.empty, 0u // TODO: Start at # of field imports when building field definitions.

    for f in fields.Definitions do
        let result =
            validated {
                let! fowner =
                    let (pos, owner) = f.FieldOwner
                    match types.FindDefinition owner with
                    | ValueSome namei -> Result.Ok namei
                    | ValueNone -> Result.Error [ errorIdentifierUndefined "field owner type definition" owner pos ]

                let others =
                    match fieldOwnerLookup.TryGetValue fowner with
                    | true, existing -> existing
                    | false, _ ->
                        let m = ImmutableArray.CreateBuilder()
                        fieldOwnerLookup.Add(fowner, m)
                        m

                others.Add(FieldIndex.Index i)

                let! fname =
                    let (pos, name) = f.FieldName
                    match identifiers.FromIdentifier name with
                    | ValueSome namei -> Result.Ok namei
                    | ValueNone -> Result.Error [ errorIdentifierUndefined "field name" name pos ]

                let! ftype =
                    let (pos, name) = f.FieldType
                    match signatures.FromIdentifier name with
                    | ValueSome typei -> Result.Ok typei
                    | ValueNone -> Result.Error [ errorIdentifierUndefined "field type" name pos ]

                return! Result.Ok
                    { Field.FieldOwner = fowner
                      FieldName = fname
                      FieldVisibility = f.FieldVisibility
                      FieldFlags = f.FieldFlags
                      FieldType = ftype
                      FieldAnnotations = ImmutableArray.Empty }
            }

        match result with
        | Result.Ok m' -> builder.Add m'
        | Result.Error errors' -> errors <- errors' @ errors

    match errors with
    | [] -> Result.Ok(builder.ToImmutable())
    | _ -> Result.Error errors

let buildMethodDefinitions
    (identifiers: IdentifierLookup)
    (signatures: SymbolDictionary<_, UnresolvedMethodSignature>)
    (code: SymbolDictionary<_, _>)
    (types: TypeDefinitionLookup)
    (methods: MethodLookup)
    (methodOwnerLookup: IDictionary<_, _>)
    =
    let imports = ImmutableArray.CreateBuilder methods.Imports.Count
    let definitions = ImmutableArray.CreateBuilder methods.Definitions.Count
    let mutable errors, i = List.empty, 0u

    for m in methods.Imports do
        let result = validated {
            let! mowner =
                let (pos, owner) = m.MethodOwner
                match types.Find owner with
                | ValueSome namei -> Result.Ok namei
                | ValueNone -> Result.Error [ errorIdentifierUndefined "method import owner type definition" owner pos ]

            let! mname =
                let (pos, name) = m.MethodName
                match identifiers.FromIdentifier name with
                | ValueSome namei -> Result.Ok namei
                | ValueNone -> Result.Error [ errorIdentifierUndefined "method name" name pos ]

            let! msig =
                let (pos, signature) = m.MethodSignature
                match signatures.FromIdentifier signature with
                | ValueSome namei -> Result.Ok namei
                | ValueNone -> Result.Error [ errorIdentifierUndefined "method signature" signature pos ]

            return! Result.Ok
                { MethodImport.MethodOwner = mowner
                  MethodName = mname
                  TypeParameters = 0u
                  Signature = msig }
        }

        match result with
        | Result.Ok m' -> imports.Add m'
        | Result.Error errors' -> errors <- errors' @ errors

        i <- Checked.(+) i 1u

    for m in methods.Definitions do
        let result =
            validated {
                let! mowner =
                    let (pos, owner) = m.MethodOwner
                    match types.FindDefinition owner with
                    | ValueSome namei -> Result.Ok namei
                    | ValueNone -> Result.Error [ errorIdentifierUndefined "method owner type definition" owner pos ]

                let others =
                    match methodOwnerLookup.TryGetValue mowner with
                    | true, existing -> existing
                    | false, _ ->
                        let m = ImmutableArray.CreateBuilder()
                        methodOwnerLookup.Add(mowner, m)
                        m

                others.Add(MethodIndex.Index i)

                let! mname =
                    match m.MethodName with
                    | ValueSome(pos, name) ->
                        match identifiers.FromIdentifier name with
                        | ValueSome namei -> Result.Ok namei
                        | ValueNone -> Result.Error [ errorIdentifierUndefined "method name" name pos ]
                    | ValueNone -> Result.Ok emptyIdentifierIndex

                let! msig =
                    let (pos, signature) = m.MethodSignature
                    match signatures.FromIdentifier signature with
                    | ValueSome namei -> Result.Ok namei
                    | ValueNone -> Result.Error [ errorIdentifierUndefined "method signature" signature pos ]

                let! mbody = Result.mapError List.singleton (m.MethodBody code.FromIdentifier)

                return! Result.Ok
                    { Method.MethodOwner = mowner
                      MethodName = mname
                      MethodVisibility = m.MethodVisibility
                      MethodFlags = m.MethodFlags
                      TypeParameters = ImmutableArray.Empty
                      Signature = msig
                      MethodAnnotations = ImmutableArray.Empty
                      Body = mbody }
            }

        match result with
        | Result.Ok m' -> definitions.Add m'
        | Result.Error errors' -> errors <- errors' @ errors

        i <- Checked.(+) i 1u

    match errors with
    | [] -> Result.Ok(imports.ToImmutable(), definitions.ToImmutable())
    | _ -> Result.Error errors

let buildCodeRegisters (types: TypeSignatureLookup) =
    let localRegisterBuilder = ImmutableArray.CreateBuilder<struct(uvarint * RegisterType)>()

    let addCodeRegister typei flags =
        let rtype =
            { RegisterType.RegisterType = typei
              RegisterFlags = flags }

        let inline addNewRegister() = localRegisterBuilder.Add(struct(1u, rtype))

        if localRegisterBuilder.Count > 0 then
            let currenti = localRegisterBuilder.Count - 1
            let struct(prevc, prevt) = localRegisterBuilder.[currenti]
            if prevt.RegisterType = rtype.RegisterType && prevt.RegisterFlags = rtype.RegisterFlags
            then localRegisterBuilder.[currenti] <- struct(Checked.(+) prevc 1u, prevt)
            else addNewRegister()
        else addNewRegister()

    let errorUnresolvedType rname (pos, t) =
        ValidationError (
            "Unable to find register type of the register $" + string rname +
            ", the type signature corresponding to the identifier $" + string t + " could not be found",
            pos
        )

    let buildArgumentRegisters =
        let rec inner
            (lookup: Dictionary<Name, _>)
            (indices: HashSet<_>)
            (names: HashSet<_>)
            (locals: List<_>)
            (registers: UnresolvedCodeRegister list)
            errors
            =
            match registers, errors with
            | [], [] -> Result.Ok(lookup, locals)
            | [], _ -> Result.Error errors
            | { RegisterName = rpos, rname as rname'; RegisterType = rtype } :: remaining, _ ->
                let errors' =
                    if names.Add rname then
                        match rtype with
                        | UnresolvedRegisterType.Argument(Index i as rindex) ->
                            if indices.Add rindex then
                                lookup.Add(rname, rindex)
                                errors
                            else
                                let e =
                                    ValidationError (
                                        "An identifier corresponding to argument register #" + string i + " already exists",
                                        rpos
                                    )
                                e :: errors
                        | UnresolvedRegisterType.Local(lpos, lname) ->
                            locals.Add(struct(rname', (lpos, lname)))
                            errors
                    else
                        errorDuplicateIdentifier "register" rname rpos :: errors
                inner lookup indices names locals remaining errors'
        fun registers -> inner (Dictionary()) (HashSet()) (HashSet()) (List()) registers List.empty

    let rec inner i (lookup: Dictionary<Name, RegisterIndex>) (locals: List<_>) errors =
        if i < locals.Count then
            let errors' =
                let struct((rpos, rname), (tpos, tname)) = locals.[i]
                match types.FromIdentifier tname with
                | ValueSome typei ->
                    if lookup.TryAdd(rname, Index(uint32 lookup.Count)) then
                        addCodeRegister typei RegisterFlags.None
                        errors
                    else errorDuplicateIdentifier "register" rname rpos :: errors
                | ValueNone ->
                    errorUnresolvedType rname (tpos, tname) :: errors
            inner (i + 1) lookup locals errors'
        else
            match errors with
            | [] -> Result.Ok(lookup :> IReadOnlyDictionary<_, _>, localRegisterBuilder.ToImmutable())
            | _ -> Result.Error errors

    fun registers ->
        localRegisterBuilder.Clear()

        match buildArgumentRegisters registers with
        | Result.Ok(lookup, locals) ->
            inner 0 lookup locals List.empty
        | Result.Error errors -> Result.Error errors

let buildCodeInstructions fields methods =
    let builder = ImmutableArray.CreateBuilder()

    fun (registers: IReadOnlyDictionary<_, _>) ->
        let lookupRegisterName name =
            match registers.TryGetValue name with
            | true, rindex -> ValueSome rindex
            | false, _ -> ValueNone

        let rec inner instrs errors =
            match instrs, errors with
            | [], [] -> Result.Ok(builder.ToImmutable())
            | [], _ -> Result.Error errors
            | (instr: UnresolvedInstruction) :: remaining, _ ->
                let errors' =
                    match instr lookupRegisterName fields methods with
                    | Result.Ok instr' ->
                        builder.Add instr'
                        errors
                    | Result.Error err -> err @ errors
                inner remaining errors'

        fun instrs ->
            builder.Clear()
            inner instrs List.empty

let tmodule: Parser<AssemblerResult, State> =
    let moduleVersionInfo vname version err =
        getPosition
        .>> keyword vname
        .>>. vnumbers
        >>= fun (pos, numbers) ->
            updateUserState <| fun state ->
                match version state.Module with
                | { contents = ValueNone } as ver ->
                    ver.contents <- ValueSome numbers
                    state
                | { contents = ValueSome _ } ->
                    { state with Errors = ValidationError(err, pos) :: state.Errors }

    choice [
        moduleVersionInfo "format" (fun mdle -> mdle.ModuleFormatVersion) "Duplicate module format version"
        moduleVersionInfo "version" (fun mdle -> mdle.ModuleHeaderVersion) "Duplicate module version"
        tidentifier
        tsignature
        tcode
        // "namespace" also starts with "name", so this parser must go before the module name
        tnamespace

        keyword "name" >>. getPosition .>>. stringlit >>= fun (pos, mname) ->
            updateUserState <| fun state ->
                match Name.tryOfStr mname, state.Module.ModuleHeaderName with
                | ValueSome mname', ValueNone ->
                    state.Module.ModuleHeaderName <- ValueSome mname'
                    state
                | _, ValueSome _ ->
                    { state with Errors = ValidationError("Duplicate module name", pos) :: state.Errors }
                | ValueNone, _ ->
                    { state with Errors = ValidationError("The module name cannot be empty", pos) :: state.Errors }

        timports
        ttypedef
        tfielddef
        tmethoddef
        tctordef

        keyword "entrypoint" >>. getPosition .>>. identifier >>= fun ((pos, _) as ename) ->
            updateUserState <| fun state ->
                match state.Module.ModuleEntryPoint with
                | ValueNone ->
                    state.Module.ModuleEntryPoint <- ValueSome ename
                    state
                | ValueSome _ ->
                    { state with Errors = ValidationError("Duplicate module entry point", pos) :: state.Errors }
    ]
    |> sexpression
    |> skipMany
    |> declaration "module" // TODO: Have way to refer to current module $
    >>. eof
    >>. getUserState
    |>> fun { Module = state; Errors = errors } ->
        match state.ModuleHeaderName, errors with
        | ValueSome mname, [] ->
            validated {
                let! namespaces = tryMapLookup state.ModuleNamespaces (buildLookupValues state.ModuleIdentifiers)

                let! tsignatures =
                    let tryFindType = state.ModuleTypes.Find
                    tryMapLookup state.ModuleTypeSignatures (fun t -> t tryFindType |> Result.mapError List.singleton)

                let! msignatures =
                    let getTypeList = buildLookupValues state.ModuleTypeSignatures
                    tryMapLookup state.ModuleMethodSignatures <| fun m ->
                        validated {
                            let! rtypes = getTypeList m.ReturnTypes
                            let! ptypes = getTypeList m.ParameterTypes
                            return! Result.Ok { MethodSignature.ReturnTypes = rtypes; ParameterTypes = ptypes }
                        }

                let fieldOwnerLookup = Dictionary state.ModuleTypes.Definitions.Count

                let! fdefinitions =
                    buildFieldDefinitions
                        state.ModuleIdentifiers
                        state.ModuleTypeSignatures
                        state.ModuleTypes
                        state.ModuleFields
                        fieldOwnerLookup

                let methodOwnerLookup = Dictionary state.ModuleTypes.Definitions.Count

                let! (mimports, mdefinitions) =
                    buildMethodDefinitions
                        state.ModuleIdentifiers
                        state.ModuleMethodSignatures
                        state.ModuleCode
                        state.ModuleTypes
                        state.ModuleMethods
                        methodOwnerLookup

                let! (timports, tdefinitions) =
                    buildTypeDefinitions
                        state.ModuleIdentifiers
                        state.ModuleImports
                        state.ModuleNamespaces
                        state.ModuleTypes methodOwnerLookup
                        fieldOwnerLookup

                let! codes =
                    let getCodeRegisters = buildCodeRegisters state.ModuleTypeSignatures
                    let getCodeInstructions = buildCodeInstructions state.ModuleFields state.ModuleMethods
                    tryMapLookup state.ModuleCode <| fun code ->
                        validated {
                            let! (rlookup, registers) = getCodeRegisters code.Registers
                            let! instructions = getCodeInstructions rlookup code.Instructions
                            return! Result.Ok { Code.RegisterTypes = registers; Instructions = instructions }
                        }

                let! entrypoint =
                    match state.ModuleEntryPoint with
                    | ValueSome(pos, ename) ->
                        match state.ModuleMethods.FindDefinition ename with
                        | ValueSome _ as eindex ->
                            Result.Ok eindex
                        | ValueNone ->
                            Result.Error [ ValidationError("Unable to find entry point method $" + string ename, pos) ]
                    | ValueNone -> Result.Ok ValueNone

                return! Result.Ok
                    { Module.Magic = magic
                      FormatVersion = ValueOption.defaultValue currentFormatVersion state.ModuleFormatVersion.contents
                      Header =
                        { ModuleHeader.Module =
                            { ModuleIdentifier.ModuleName = mname
                              Version = ValueOption.defaultValue VersionNumbers.empty state.ModuleHeaderVersion.contents }
                          // TODO: Allow setting of module header flags.
                          Flags = ModuleHeaderFlags.LittleEndian
                          // TODO: Allow setting of pointer size in module header.
                          PointerSize = PointerSize.Unspecified }
                      Identifiers = { IdentifierSection.Identifiers = ImmutableArray.CreateRange state.ModuleIdentifiers.Values }
                      Namespaces = namespaces
                      TypeSignatures = tsignatures
                      MethodSignatures = msignatures
                      Imports =
                        // TODO: Implement generation of module imports
                        { ModuleImports.ImportedModules = ImmutableArray.CreateRange state.ModuleImports.Imports
                          ImportedTypes = timports
                          ImportedFields = ImmutableArray.Empty
                          ImportedMethods = mimports }
                      Definitions =
                        { ModuleDefinitions.DefinedTypes = tdefinitions
                          ModuleDefinitions.DefinedFields = fdefinitions
                          ModuleDefinitions.DefinedMethods = mdefinitions }
                      // TODO: Implement generation of data
                      Data = ImmutableArray.Empty
                      Code = codes
                      EntryPoint = entrypoint
                      Debug = () }
            }
        | ValueSome _, _ ->
            Result.Error errors
        | ValueNone, _ ->
            failwith "TODO: Error for module must have a name"

let fromInput (input: _ -> _ -> ParserResult<AssemblerResult, State>) =
    let result =
        { State.Module =
            { IncompleteModule.ModuleFormatVersion = ref ValueNone
              ModuleHeaderName = ValueNone
              ModuleHeaderVersion = ref ValueNone
              ModuleIdentifiers =
                let identifiers = SymbolDictionary()
                identifiers.AddAnonymous String.Empty |> ignore
                identifiers
              ModuleNamespaces = SymbolDictionary()
              ModuleTypeSignatures = TypeSignatureLookup()
              ModuleMethodSignatures = SymbolDictionary()
              ModuleImports = ModuleImportLookup()
              ModuleTypes = TypeDefinitionLookup()
              ModuleFields = FieldLookup()
              ModuleMethods = MethodLookup()
              ModuleCode = SymbolDictionary()
              ModuleEntryPoint = ValueNone }
          Errors = List.empty }
        |> input tmodule

    match result with
    | Success(result, _, _) -> result
    | Failure(msg, err, _) -> AssemblerResult.Error [ ParserError(msg, err) ]
