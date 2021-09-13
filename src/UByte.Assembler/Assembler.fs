module UByte.Assembler.Assembler

open System
open System.Collections.Generic
open System.Collections.Immutable

open UByte.Format.Model

type Position = FParsec.Position

let atomErrorMsg =
    let format = sprintf "\"%O\" at %O"
    fun { Parser.Atom = atom; Parser.Position = pos } -> format atom pos

type AssemblerError =
    | ExpectedKeyword of expected: string * actual: Parser.PositionedAtom option
    | DuplicateFormatVersion of duplicate: Parser.PositionedAtom
    | InvalidVersionNumber of number: Parser.PositionedAtom
    | MissingModuleName of name: Parser.PositionedAtom
    | DuplicateModuleName of duplicate: Parser.PositionedAtom
    | DuplicateModuleVersion of duplicate: Parser.PositionedAtom
    | SymbolNotDefined of symbol: Name * Parser.PositionedAtom
    | EmptyIdentifierEntry of identifier: Parser.PositionedAtom
    | DuplicateIdentifierSymbol of symbol: Name * identifier: Parser.PositionedAtom
    | InvalidSignatureKind of signature: Parser.PositionedAtom
    | MissingTypeSignature of Parser.PositionedAtom
    | InvalidTypeSignature of ``type``: Parser.PositionedAtom
    | InvalidMethodSignature of method : Parser.PositionedAtom
    | UnexpectedAtom of Parser.PositionedAtom

    override this.ToString() =
        match this with
        | ExpectedKeyword(expected, actual) ->
            Option.map atomErrorMsg actual
            |> Option.defaultValue "end of file"
            |> sprintf "Expected keyword \"%s\" but got %s" expected
        | DuplicateFormatVersion { Parser.Position = pos } ->
            sprintf "Duplicate format version at %O" pos
        | InvalidVersionNumber atom ->
            atomErrorMsg atom
            + "is not a valid version number, versions are specified as a list of positive integers"
        | MissingModuleName atom ->
            atomErrorMsg atom + "is missing the module name, module names are of the form (name \"my.module.name\")"
        | DuplicateModuleName { Parser.Position = pos } ->
            sprintf "Duplicate module name at %O" pos
        | DuplicateModuleVersion { Parser.Position = pos } ->
            sprintf "Duplicate module version at %O" pos
        | SymbolNotDefined(symbol, atom) ->
            sprintf "The symbol %O in %s is not defined" symbol (atomErrorMsg atom)
        | EmptyIdentifierEntry atom ->
            atomErrorMsg atom +
            "is not a valid identifier, identifiers should be of the form (identifier $my_name_symbol \"MyName\") or " +
            "(identifier \"MyName\")"
        | DuplicateIdentifierSymbol(symbol, { Parser.Position = pos }) ->
            sprintf "Duplicate identifier at %O, an identifier corresponding to the symbol $%O already exists" pos symbol
        | InvalidSignatureKind atom ->
            atomErrorMsg atom + " is an invalid signature, a valid signature should be of the form (signature " +
            "$my_signature_symbol (type ...)) where ... indicates a type signature or (signature $my_signature_symbol (method " +
            "...)) where ... indicates a method signature"
        | MissingTypeSignature atom ->
            atomErrorMsg atom
            + "is not a valid type, a valid type should be of the form (type ...) where ... indicates a type signature"
        | InvalidTypeSignature atom ->
            atomErrorMsg atom + " is an invalid type signature, a valid type should be a primitive (s32, bool, char16, etc.)"
        | InvalidMethodSignature atom ->
            atomErrorMsg atom + " is an invalid method signature, method signatures are of the form (method (returns ...) (param"
        | UnexpectedAtom atom -> "Unexpected " + atomErrorMsg atom

[<Sealed>]
type SymbolDictionary<'IndexKind, 'T when 'IndexKind :> IndexKinds.Kind> () =
    let items = ImmutableArray.CreateBuilder<'T>()
    let symbols = Dictionary<Name, int32>()

    member _.AddAnonymous item = items.Add item

    member this.AddNamed(symbol, item) =
        let i = items.Count

        match symbols.TryGetValue symbol with
        | false, _ ->
            symbols.Add(symbol, i)
            this.AddAnonymous item
            None
        | true, _ -> Some(fun a -> DuplicateIdentifierSymbol(symbol, a))

    member this.Add(symbol, item) =
        match symbol with
        | ValueSome symbol -> this.AddNamed(symbol, item)
        | ValueNone -> this.AddAnonymous item; None

    member _.FromSymbol symbol: Index<'IndexKind> voption =
        match symbols.TryGetValue symbol with
        | true, item -> ValueSome(Index(Checked.uint32 item))
        | false, _ -> ValueNone

[<NoComparison; NoEquality>]
type State =
    { ModuleFormatVersion: VersionNumbers voption ref
      mutable ModuleHeaderName: Name voption
      ModuleHeaderVersion: VersionNumbers voption ref
      ModuleIdentifiers: SymbolDictionary<IndexKinds.Identifier, Name>
      ModuleTypeSignatures: SymbolDictionary<IndexKinds.TypeSignature, AnyType>
      ModuleMethodSignatures: SymbolDictionary<IndexKinds.MethodSignature, MethodSignature> }

let inline (|Atom|) { Parser.Atom = atom } = atom

type ExpressionParserResult = System.ValueTuple<Parser.PositionedAtom list, AssemblerError list>

let rec extraneous atoms errors =
    match atoms with
    | [] -> List.rev errors
    | extra :: remaining -> extraneous remaining (UnexpectedAtom extra :: errors)

let keyword name contents atoms (errors: AssemblerError list) (state: State): ExpressionParserResult option =
    let contents' watom nested atoms': struct(_ * _) option =
        match contents watom nested errors state with
        | struct([], errors') -> Some(atoms', errors')
        | extra, errors' -> Some(atoms', extraneous extra errors')

    match atoms with
    | Atom(Parser.Keyword word) as watom :: atoms' when word = name ->
        contents' watom List.empty atoms'
    | Atom(Parser.NestedAtom(Atom(Parser.Keyword word) as watom :: nested)) :: atoms' when word = name ->
        contents' watom nested atoms'
    | _ -> None

let keywordWithSymbol name contents =
    keyword
        name
        (fun katom atoms errors state ->
            match atoms with
            | Atom(Parser.Identifier symbol) :: remaining ->
                contents (ValueSome (Name.ofStr symbol)) katom remaining errors state
            | _ ->
                contents ValueNone katom atoms errors state)

let emptyFormatVersion _ state =
    state.ModuleFormatVersion.contents <- ValueSome VersionNumbers.empty
    None

let emptyHeaderVersion _ state =
    state.ModuleHeaderVersion.contents <- ValueSome VersionNumbers.empty
    None

let versionNumberVector field atoms errors (_: State) =
    let rec inner (numbers: ImmutableArray<_>.Builder) field atoms errors =
        match atoms with
        | [] ->
            field.contents <- ValueSome(VersionNumbers(numbers.ToImmutable()))
            struct([], errors)
        | Atom(Parser.IntegerLiteral n) :: remaining when n >= 0L ->
            numbers.Add(uint32 n)
            inner numbers field remaining errors
        | bad :: remaining ->
            inner numbers field remaining (InvalidVersionNumber bad :: errors)
    inner (ImmutableArray.CreateBuilder()) field atoms errors

let (|ParsedIdentifier|) (Atom atom) =
    match atom with
    | Parser.StringLiteral name -> Name.tryOfStr name
    | _ -> ValueNone

let (|ParsedTypeSignature|) atoms =
    let inline primitive remaining t: struct(_ * _) voption = ValueSome(remaining, AnyType.Primitive t)
    match atoms with
    | Atom(Parser.Keyword "bool") :: atoms' -> primitive atoms' PrimitiveType.Bool
    | Atom(Parser.Keyword "s8") :: atoms' -> primitive atoms' PrimitiveType.S8
    | Atom(Parser.Keyword "u8") :: atoms' -> primitive atoms' PrimitiveType.U8
    | Atom(Parser.Keyword "s16") :: atoms' -> primitive atoms' PrimitiveType.S16
    | Atom(Parser.Keyword "u16") :: atoms' -> primitive atoms' PrimitiveType.U16
    | Atom(Parser.Keyword "char16") :: atoms' -> primitive atoms' PrimitiveType.Char16
    | Atom(Parser.Keyword "s32") :: atoms' -> primitive atoms' PrimitiveType.S32
    | Atom(Parser.Keyword "u32") :: atoms' -> primitive atoms' PrimitiveType.U32
    | Atom(Parser.Keyword "char32") :: atoms' -> primitive atoms' PrimitiveType.Char32
    | Atom(Parser.Keyword "s64") :: atoms' -> primitive atoms' PrimitiveType.S64
    | Atom(Parser.Keyword "u64") :: atoms' -> primitive atoms' PrimitiveType.U64
    | Atom(Parser.Keyword "f32") :: atoms' -> primitive atoms' PrimitiveType.F32
    | Atom(Parser.Keyword "f64") :: atoms' -> primitive atoms' PrimitiveType.F64
    | Atom(Parser.Keyword "unit") :: atoms' -> primitive atoms' PrimitiveType.Unit
    | _ -> ValueNone

let (|ParsedMethodSignature|) matom errors state atoms =
    let (|ParsedMethodTypes|) name errors atoms: struct(_ * _) voption =
        match atoms with
        | Atom(Parser.NestedAtom(Atom(Parser.Keyword keyword) :: types)) as matom :: atoms' when keyword = name ->
            let rec inner (builder: ImmutableArray<_>.Builder) types errors: struct(_ * _) voption =
                match types with
                | Atom(Parser.Identifier tname) as tatom :: types' ->
                    let tname' = Name.ofStr tname
                    match state.ModuleTypeSignatures.FromSymbol tname' with
                    | ValueSome t ->
                        builder.Add t
                        inner builder types' errors
                    | ValueNone ->
                        ValueSome([], Error(SymbolNotDefined(tname', tatom) :: errors))
                | [] ->
                    ValueSome([], Ok(builder.ToImmutable()))
                | _ ->
                    ValueSome(atoms', Error(InvalidMethodSignature matom :: errors))
            inner (ImmutableArray.CreateBuilder()) types errors
        | [] as atoms'
        | Atom(Parser.Keyword _) :: atoms'
        | Atom(Parser.NestedAtom [ Atom(Parser.Keyword _) ]) :: atoms' ->
            // Keyword is not checked as list of return types might be omitted.
            ValueSome(atoms', Ok ImmutableArray.Empty)
        | _ -> ValueNone

    let invalidMethodSignature = Error(InvalidMethodSignature matom :: errors)

    match atoms with
    | ParsedMethodTypes "returns" errors (ValueSome(atoms', Ok rtypes)) ->
        match atoms' with
        | ParsedMethodTypes "parameters" errors (ValueSome([], Ok ptypes)) ->
            Ok { ReturnTypes = rtypes; ParameterTypes = ptypes }
        | ParsedMethodTypes "parameters" errors (ValueSome(_, Error errors')) ->
            Error errors'
        | _ -> invalidMethodSignature
    | _ -> invalidMethodSignature

let assemble atoms =
    let state =
        { ModuleFormatVersion = ref ValueNone
          ModuleHeaderName = ValueNone
          ModuleHeaderVersion = ref ValueNone
          ModuleIdentifiers = SymbolDictionary()
          ModuleTypeSignatures = SymbolDictionary()
          ModuleMethodSignatures = SymbolDictionary() }

    match atoms with
    | Atom(Parser.Keyword "module") :: contents ->
        let rec inner atoms errors =
            let inline (|VersionField|_|) name field err atoms =
                keyword
                    name
                    (fun fvatom atoms errors state ->
                        match field.contents with
                        | ValueNone -> versionNumberVector field atoms errors state
                        | ValueSome _ -> struct([], err fvatom :: errors))
                    atoms
                    errors
                    state

            let inline (|ModuleIdentifier|_|) atoms =
                keywordWithSymbol
                    "identifier"
                    (fun symbol iatom atoms errors state ->
                        match atoms with
                        | ParsedIdentifier(ValueSome name) :: extra ->
                            match state.ModuleIdentifiers.Add(symbol, name) with
                            | None -> struct([], extraneous extra errors)
                            | Some err -> struct([], err iatom :: errors)
                        | ParsedIdentifier ValueNone :: _
                        | [] -> struct([], EmptyIdentifierEntry iatom :: errors))
                    atoms
                    errors
                    state

            let inline (|ModuleSignature|_|) atoms =
                keywordWithSymbol
                    "signature"
                    (fun symbol satom atoms errors state ->
                        match atoms with
                        | Atom(Parser.NestedAtom(Atom(Parser.Keyword "type") :: ParsedTypeSignature t)) as tatom :: extra ->
                            match t with
                            | ValueSome([], t') ->
                                match state.ModuleTypeSignatures.Add(symbol, t') with
                                | None -> struct([], extraneous extra errors)
                                | Some err -> struct([], err satom :: errors)
                            | ValueSome(_ :: _, _)
                            | ValueNone ->
                                struct([], InvalidTypeSignature tatom :: errors)
                        | Atom(Parser.NestedAtom(Atom(Parser.Keyword "method") :: msig)) as matom :: extra ->
                            match msig with
                            | ParsedMethodSignature matom errors state (Ok msig') ->
                                match state.ModuleMethodSignatures.Add(symbol, msig') with
                                | None -> struct([], extraneous extra errors)
                                | Some err -> struct([], err satom :: errors)
                            | _ -> struct([], InvalidMethodSignature matom :: errors)
                        | _ -> struct([], InvalidSignatureKind satom :: errors))
                    atoms
                    errors
                    state

            match atoms with
            | VersionField "format" state.ModuleFormatVersion DuplicateFormatVersion (atoms', errors')
            | VersionField "version" state.ModuleHeaderVersion DuplicateModuleVersion (atoms', errors')
            | ModuleIdentifier (atoms', errors')
            | ModuleSignature (atoms', errors') ->
                inner atoms' errors'
            | Atom(Parser.NestedAtom(Atom(Parser.Keyword "name") as n :: ParsedIdentifier(ValueSome name) :: extra)) :: atoms' ->
                match state.ModuleHeaderVersion.contents with
                | ValueNone ->
                    state.ModuleHeaderName <- ValueSome name
                    inner atoms' (extraneous extra errors)
                | ValueSome _ ->
                    inner atoms' (DuplicateModuleName n :: errors)
            | Atom(Parser.Keyword "name") as name :: atoms'
            | Atom(Parser.NestedAtom(Atom(Parser.Keyword "name") as name :: _)) :: atoms' ->
                inner atoms' (MissingModuleName name :: errors)
            | unknown :: remaining ->
                inner remaining (UnexpectedAtom unknown :: errors)
            | [] -> errors

        match inner contents List.empty with
        | [] ->
            Ok(failwith "TODO: Make module": Module)
        | errors -> Error errors
    | _ ->
        Error [ ExpectedKeyword("module", List.tryHead atoms) ]
