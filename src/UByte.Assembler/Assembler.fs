module UByte.Assembler.Assembler

open System
open System.Collections.Immutable

open UByte.Format.Model

type Position = FParsec.Position

let atomErrorMsg =
    let format = sprintf "\"%O\" at %O"
    fun { Parser.Atom = atom; Parser.Position = pos } -> format atom pos

type AssembleError = // TODO: Rename to AssemblerError
    | ExpectedKeyword of expected: string * actual: Parser.PositionedAtom option
    | DuplicateFormatVersion of duplicate: Parser.PositionedAtom
    | InvalidVersionNumber of number: Parser.PositionedAtom
    | MissingModuleName of name: Parser.PositionedAtom
    | DuplicateModuleName of duplicate: Parser.PositionedAtom
    | DuplicateModuleVersion of duplicate: Parser.PositionedAtom
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
        | UnexpectedAtom atom -> "Unexpected " + atomErrorMsg atom

[<NoComparison; NoEquality>]
type State =
    { ModuleFormatVersion: VersionNumbers voption ref
      mutable ModuleHeaderName: Name voption
      ModuleHeaderVersion: VersionNumbers voption ref }

let inline (|Atom|) { Parser.Atom = atom } = atom

type AssemblerResult<'T> = System.ValueTuple<Result<struct('T * AssembleError list), AssembleError list>, Parser.PositionedAtom list>

type Assembler<'T> = Parser.PositionedAtom list -> AssembleError list -> State -> AssemblerResult<'T>

let ret value: Assembler<'T> = fun atoms errors _ -> Ok(value, errors), atoms

let errorExtraAtoms atoms errors =
    let rec inner atoms errors =
        match atoms with
        | [] -> errors
        | extra :: next -> inner next (UnexpectedAtom extra :: errors)
    inner atoms errors

let (.>>) (a: Assembler<'A>) (b: Assembler<'B>): Assembler<'A> =
    fun atoms errs state ->
        match a atoms errs state with
        | Ok(result, errs'), atoms' ->
            match b atoms' errs' state with
            | Ok(_, errs''), atoms'' -> Ok(result, errs''), atoms''
            | Error errs'', atoms'' -> Error errs'', atoms''
        | Error errs', atoms' -> Error errs', atoms'

let (>>.) (a: Assembler<'A>) (b: Assembler<'B>): Assembler<'B> =
    fun atoms errs state ->
        match a atoms errs state with
        | Ok(_, errs'), atoms' -> b atoms' errs' state
        | Error errs', atoms' -> Error errs', atoms'

let pipe2 (a: Assembler<'A>) (b: Assembler<'B>) pipe: Assembler<'C> =
    fun atoms errs state ->
        match a atoms errs state with
        | Ok(resulta, errs'), atoms' ->
            match b atoms' errs' state with
            | Ok(resultb, errs''), atoms'' -> Ok(pipe resulta resultb, errs''), atoms''
            | Error errs'', atoms'' -> Error errs'', atoms''
        | Error errs', atoms' -> Error errs', atoms'

let (.>>.) a b = pipe2 a b (fun a' b' -> a', b')

let (|>>) (parser: Assembler<'T>) (mapping: 'T -> 'U): Assembler<'U> =
    fun atoms errs state ->
        match parser atoms errs state with
        | Ok(result, errs'), atoms' -> Ok(mapping result, errs'), atoms'
        | Error errs', atoms' -> Error errs', atoms'

let tryUpdateState updater: Assembler<'T> =
    fun atoms errors state ->
        match updater state with
        | Ok result -> Ok(result, errors), atoms
        | Error err -> Error(err :: errors), atoms

let tryMap (mapping: 'T -> Result<'U, AssembleError>) (parser: Assembler<'T>): Assembler<'U> =
    fun atoms errs state ->
        match parser atoms errs state with
        | Ok(result, errs'), atoms' ->
            match mapping result with
            | Ok result' -> Ok(result', errs'), atoms'
            | Error err -> Error(err :: errs'), atoms'
        | Error errs', atoms' -> Error errs', atoms'

let keyword name: Assembler<unit> =
    fun atoms errors _ ->
        match atoms with
        | [] -> Error(ExpectedKeyword(name, None) :: errors), atoms
        | Atom(Parser.Keyword actual) :: rest when StringComparer.Ordinal.Equals(name, actual) -> Ok((), errors), rest
        | bad :: rest -> Error(ExpectedKeyword(name, Some bad) :: errors), rest

let choice (parsers: Assembler<'T> list): Assembler<'T> =
    let rec inner (last: AssemblerResult<_>) (parsers: Assembler<'T> list) atoms errors state =
        match parsers with
        | [] -> last
        | current :: remaining ->
            match current atoms errors state with
            | Ok(result, errs'), atoms' ->
                struct(Ok(result, errs'), atoms')
            | Error errs', atoms' ->
                inner (Error errs', atoms') remaining atoms errors state

    fun atoms -> inner (struct(Error List.empty, atoms)) parsers atoms

let many (parser: Assembler<'T>): Assembler<ImmutableArray<'T>> =
    let rec inner (items: ImmutableArray<'T>.Builder) atoms errors state: AssemblerResult<_> =
        match parser atoms errors state with
        | (Ok(_, errors') as result), atoms'
        | (Error errors' as result), atoms' ->
            match result with
            | Ok(result', _) -> items.Add result'
            | Error _ -> ()

            match atoms' with
            | _ :: _ -> inner items atoms' errors' state
            | [] -> Ok(items.ToImmutable(), errors'), atoms'
    fun atoms -> inner (ImmutableArray.CreateBuilder()) atoms

// TODO: Make a skipMany

let directive name empty (contents: _ -> Assembler<'T>): Assembler<'T> =
    fun atoms errors state ->
        match atoms with
        | Atom(Parser.Keyword keyword) as atom :: atoms' when keyword = name ->
            match empty atom with
            | Ok result -> Ok(result, errors), atoms'
            | Error err -> Error(err :: errors), atoms'
        | Atom(Parser.NestedAtom(Atom(Parser.Keyword keyword) as atom :: contents')) :: atoms' when keyword = name ->
            match contents atom contents' errors state with
            | Ok(result, errors'), [] -> Ok(result, errors'), atoms'
            | Ok(_, errors'), extra -> Error(errorExtraAtoms extra errors'), atoms'
            | Error errors', _ -> Error errors', atoms'
        | Atom(Parser.Keyword _) as atom :: atoms'
        | Atom(Parser.NestedAtom(Atom(Parser.Keyword _) as atom :: _)) :: atoms' ->
            Error(ExpectedKeyword(name, Some atom) :: errors), atoms'
        | _ ->
            Error(ExpectedKeyword(name, None) :: errors), atoms

let versionNumberList: Assembler<VersionNumbers> =
    let rec inner (numbers: ImmutableArray<_>.Builder) atoms errors: AssemblerResult<_> =
        match atoms with
        | [] -> Ok(numbers.ToImmutable() |> VersionNumbers, errors), atoms
        | Atom(Parser.IntegerLiteral n) :: numbers' when n >= 0L ->
            numbers.Add(uint32 n)
            inner numbers numbers' errors
        | bad :: _ ->
            Error(InvalidVersionNumber bad :: errors), atoms
    fun atoms errors _ -> inner (ImmutableArray.CreateBuilder()) atoms errors

let versionDirective name =
    directive name (fun atom -> Ok(atom, VersionNumbers.empty)) (fun atom -> ret atom .>>. versionNumberList)

let moduleVersionDirective name (getVersionField: _ -> _ ref) err: Assembler<_> =
    fun atoms errors state ->
        match versionDirective name atoms errors state with
        | Ok((atom, version), errors'), atoms' ->
            let update = getVersionField state
            match update.contents with
            | ValueNone ->
                update.contents <- ValueSome version
                Ok((), errors'), atoms'
            | ValueSome _ ->
                Ok((), err atom :: errors'), atoms'
        | Error errors', atoms' -> Error errors', atoms'

let assemblerEntryPoint: Assembler<unit> =
    let directives =
        choice [
            moduleVersionDirective "format" (fun state -> state.ModuleFormatVersion) DuplicateFormatVersion

            moduleVersionDirective "version" (fun state -> state.ModuleHeaderVersion) DuplicateModuleVersion
            // TODO: Fix unknown atom causes rest of parsing to end early.
        ]

    keyword "module" .>> many directives // TODO: use skipMany

let assemble atoms =
    let state =
        { ModuleFormatVersion = ref ValueNone
          ModuleHeaderName = ValueNone
          ModuleHeaderVersion = ref ValueNone }

    match assemblerEntryPoint atoms List.empty state with
    | Ok((), []), [] ->
        Ok(failwith "bad": Module)
    | Ok((), errors), extra
    | Error errors, extra ->
        Error(errorExtraAtoms extra errors)
