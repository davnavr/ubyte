module UByte.Assembler.Assemble

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FParsec

open UByte.Format.Model

// TODO: Use |>> and FParsec to make assembler easier

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

    member _.GetEnumerator() = items.GetEnumerator()

[<NoComparison; NoEquality>]
type UnresolvedTypeDefinition =
    { TypeName: Name
      TypeNamespace: Name
      TypeVisibility: VisibilityFlags
      TypeKind: TypeDefinitionLookup -> TypeDefinitionKind }

and [<Sealed>] TypeDefinitionLookup () =
    let defined = SymbolDictionary<IndexKinds.TypeDefinition, UnresolvedTypeDefinition>()
    //let imported = SymbolDictionary<IndexKinds.TypeDefinition, TypeDefinitionImport>()

    member _.AddDefinition(name, item) = defined.Add(name, item)

[<NoComparison; NoEquality>]
type UnresolvedMethod =
    { MethodOwner: Name
      MethodName: Name
      MethodVisibility: VisibilityFlags
      MethodFlags: MethodFlags
      MethodSignature: Name
      MethodBody: (Name -> CodeIndex voption) -> Result<MethodBody, AssemblerError> }

[<Sealed>]
type MethodLookup () =
    let defined = SymbolDictionary<IndexKinds.Method, UnresolvedMethod>()
    //let imported

    member _.AddDefinition(name, item) = defined.Add(name, item)
    member _.FindDefinition name = defined.FromIdentifier name
    member _.ToVector() = defined.ToVector()

type TypeSignatureLookup = SymbolDictionary<IndexKinds.TypeSignature, TypeDefinitionLookup -> AnyType>

[<NoComparison; NoEquality; IsReadOnly; Struct>]
type UnresolvedMethodSignature = { ReturnTypes: Name list; ParameterTypes: Name list }

type UnresolvedInstruction = (Name -> RegisterIndex voption) -> MethodLookup -> Result<InstructionSet.Instruction, AssemblerError list>

[<NoComparison; NoEquality; IsReadOnly; Struct>]
type UnresolvedCode =
    { Registers: (Name * Name) list
      Instructions: UnresolvedInstruction list }

[<NoComparison; NoEquality>]
type IncompleteModule =
    { ModuleFormatVersion: VersionNumbers voption ref
      mutable ModuleHeaderName: Name voption
      ModuleHeaderVersion: VersionNumbers voption ref
      ModuleIdentifiers: SymbolDictionary<IndexKinds.Identifier, string>
      ModuleNamespaces: SymbolDictionary<IndexKinds.Namespace, Name list>
      ModuleTypeSignatures: TypeSignatureLookup
      ModuleMethodSignatures: SymbolDictionary<IndexKinds.MethodSignature, UnresolvedMethodSignature>
      ModuleTypes: TypeDefinitionLookup
      //ModuleFields: 
      ModuleMethods: MethodLookup
      ModuleCode: SymbolDictionary<IndexKinds.Code, UnresolvedCode>
      mutable ModuleEntryPoint: Name voption }

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

let inline attributes flags =
    List.map (fun (name, flag) -> keyword name >>. preturn flag) flags
    |> choice
    |> many
    |>> List.fold (fun flags flag -> flags ||| flag) Unchecked.defaultof<_>

let inline flags name flags = declaration name (attributes flags)

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
        |>> (fun prim -> fun (_: TypeDefinitionLookup) -> AnyType.Primitive prim)
    ]

let tmethodsig =
    let typelist name =
        many identifier
        |> declaration name
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
        tuple2 identifier identifier
        |> declaration "register"
        |> many

    let mapRegisterNames lookup names =
        let rec inner (registers: ImmutableArray<RegisterIndex>.Builder) errors names =
            match errors, names with
            | _ :: _, [] -> Result.Error errors
            | [], [] -> Result.Ok(registers.ToImmutable())
            | _, (pos, reg) :: remaining ->
                let errors' =
                    match lookup reg with
                    | ValueSome i ->
                        registers.Add i
                        errors
                    | ValueNone ->
                        errorIdentifierUndefined "register" reg pos :: errors
                inner registers errors' remaining
        inner (ImmutableArray.CreateBuilder()) List.empty names

    let body: Parser<UnresolvedInstruction list, _> =
        choice [
            keyword "ret" >>. preturn (fun _ _ -> Result.Ok(InstructionSet.Ret ImmutableArray.Empty))

            many (getPosition .>>. identifier)
            |> declaration "ret"
            |>> fun names -> fun registers _ -> Result.map InstructionSet.Ret (mapRegisterNames registers names)
        ]
        |> many

    keyword "code" >>. tuple4 getPosition identifier registers body >>= fun (pos, id, regs, instrs) ->
        updateUserState <| fun state ->
            if state.Module.ModuleCode.AddNamed(id, { UnresolvedCode.Registers = regs; Instructions = instrs }) then
                state
            else
                { state with Errors = errorDuplicateIdentifier "code block" id pos :: state.Errors }

let tnamespace =
    tuple3 (keyword "namespace" >>. getPosition) identifier (declaration "name" (many identifier)) >>= fun (pos, id, name) ->
        updateUserState <| fun state ->
            if state.Module.ModuleNamespaces.AddNamed(id, name) then
                state
            else
                { state with Errors = errorDuplicateIdentifier "method signature" id pos :: state.Errors }

let tnamedecl = declaration "name" identifier

let tvisibility =
    flags "visibility" [
        "public", VisibilityFlags.Public
        "private", VisibilityFlags.Private
    ]

let ttypedef =
    let kind: Parser<_, _> = choice [
        choice [
            keyword "base" >>. preturn (fun (_: TypeDefinitionLookup) -> ValueNone)

            //declaration "extends"
        ]
        .>>. attributes List.empty //[ "abstract", ClassDefinitionFlags.Abstract ]
        |> declaration "class"
        |>> fun (extends, flags) -> fun lookup -> TypeDefinitionKind.Class(extends lookup, flags)
    ]

    tuple5
        (keyword "type" >>. getPosition .>>. identifier)
        tnamedecl
        (declaration "namespace" identifier)
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

let tmethoddef =
    let kind: Parser<_, _> = choice [
        getPosition .>>. identifier
        |>> fun (pos, name) -> fun lookup ->
            match lookup name with
            | ValueSome i -> Result.Ok(MethodBody.Defined i)
            | ValueNone -> Result.Error(errorIdentifierUndefined "code block" name pos)
        |> declaration "defined"
    ]

    let mflags: Parser<_, _> = flags "flags" [
        "instance", MethodFlags.Instance
    ]

    tuple5
        (tuple3 (keyword "method" >>. getPosition) identifier tnamedecl)
        tvisibility
        (declaration "signature" identifier)
        (kind .>>. mflags)
        (declaration "owner" identifier)
    >>= fun ((pos, id, name), vis, signature, (mkind, mflags), owner) ->
        updateUserState <| fun state ->
            let mdef =
                { UnresolvedMethod.MethodOwner = owner
                  MethodName = name
                  MethodVisibility = vis
                  MethodFlags = mflags
                  MethodSignature = signature
                  MethodBody = mkind }

            if state.Module.ModuleMethods.AddDefinition(ValueSome id, mdef) then
                state
            else
                { state with Errors = errorDuplicateIdentifier "method definition" id pos :: state.Errors }

type AssemblerResult = Result<Module, AssemblerError list>

let tmodule: Parser<AssemblerResult, State> =
    let moduleVersionInfo vname version err =
        getPosition
        .>> keyword vname
        .>>. (many integerlit |>> (List.map uint32 >> ImmutableArray.CreateRange >> VersionNumbers))
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

        tidentifier

        tsignature

        tcode

        tnamespace

        ttypedef

        tmethoddef

        keyword "entrypoint" >>. getPosition .>>. identifier >>= fun (pos, ename) ->
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
    |> declaration "module"
    >>. eof
    >>. getUserState
    |>> fun { Module = state; Errors = errors } ->
        match state.ModuleHeaderName, errors with
        | ValueSome mname, [] ->
            

            failwith "bad"

let fromInput (input: _ -> _ -> ParserResult<AssemblerResult, State>) =
    let result =
        { State.Module =
            { IncompleteModule.ModuleFormatVersion = ref ValueNone
              ModuleHeaderName = ValueNone
              ModuleHeaderVersion = ref ValueNone
              ModuleIdentifiers = SymbolDictionary()
              ModuleNamespaces = SymbolDictionary()
              ModuleTypeSignatures = TypeSignatureLookup()
              ModuleMethodSignatures = SymbolDictionary()
              ModuleTypes = TypeDefinitionLookup()
              ModuleMethods = MethodLookup()
              ModuleCode = SymbolDictionary()
              ModuleEntryPoint = ValueNone }
          Errors = List.empty }
        |> input tmodule

    match result with
    | Success(result, _, _) -> result
    | Failure(msg, err, _) -> AssemblerResult.Error [ ParserError(msg, err) ]
