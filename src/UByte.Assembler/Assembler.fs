module UByte.Assembler.Assembler

open System
open System.Collections.Immutable

open UByte.Format.Model

type Position = FParsec.Position

let atomErrorMsg =
    let format = sprintf "\"%O\" at %O"
    fun { Parser.Atom = atom; Parser.Position = pos } -> format atom pos

type AssembleError =
    | ExpectedKeyword of expected: string * actual: Parser.PositionedAtom option
    | DuplicateFormatVersion of duplicate: Parser.PositionedAtom
    | InvalidVersionNumber of Parser.PositionedAtom
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
            atomErrorMsg atom + "is not a valid version number"
        | UnexpectedAtom atom -> "Unexpected " + atomErrorMsg atom

let inline (|Atom|) { Parser.Atom = atom } = atom

let keyword name atoms: struct(_ * _) =
    match atoms with
    | [] -> Error(ExpectedKeyword(name, None)), atoms
    | Atom(Parser.Keyword actual) :: rest when StringComparer.Ordinal.Equals(name, actual) -> Ok(), rest
    | bad :: rest -> Error(ExpectedKeyword(name, Some bad)), rest

let versionNumberList atoms =
    let rec inner (numbers: ImmutableArray<_>.Builder) atoms =
        match atoms with
        | [] ->
            Ok(numbers.ToImmutable())
        | Atom(Parser.IntegerLiteral n) :: numbers' when n >= 0L ->
            numbers.Add(uint32 n)
            inner numbers numbers'
        | bad :: _ ->
            Error(InvalidVersionNumber bad)
    inner (ImmutableArray.CreateBuilder()) atoms

let assemble (atoms: Parser.PositionedAtom list) =
    match keyword "module" atoms with
    | Ok(), atoms' ->
        let mutable formatVersionNumbers = ValueNone

        let rec declarations atoms errs =
            let inline setFormatVersion atom fversion =
                match formatVersionNumbers with
                | ValueSome _ -> DuplicateFormatVersion atom :: errs
                | ValueNone ->
                    formatVersionNumbers <- ValueSome fversion
                    errs

            match atoms with
            | Atom(Parser.Keyword "format") as fversion :: next ->
                declarations next (setFormatVersion fversion ImmutableArray.Empty)
            | Atom(Parser.NestedAtom(Atom(Parser.Keyword "format") :: numbers)) as fversion :: next ->
                match versionNumberList numbers with
                | Ok numbers' -> declarations next (setFormatVersion fversion numbers')
                | Error err -> declarations next (err :: errs)
            | Atom(Parser.NestedAtom [ Atom(Parser.NestedAtom _) as nested ]) :: next -> // TODO: Check that this can parse ((double nested)) things.
                declarations (nested :: next) errs
            | bad :: next ->
                declarations next (UnexpectedAtom bad :: errs)
            | [] -> errs

        match declarations atoms' List.empty with
        | [] ->
            Ok(failwith "TODO: Build the module": Module)
        | errs -> Error errs
    | Error err, _ -> Error [ err ]
