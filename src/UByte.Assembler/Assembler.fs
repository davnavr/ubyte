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

let extraneous atoms errors =
    let rec inner atoms errors =
        match atoms with
        | [] -> errors
        | extra :: next -> inner next (UnexpectedAtom extra :: errors)
    inner atoms errors

let assemble atoms =
    match keyword "module" atoms with
    | Ok(), atoms' ->
        let mutable moduleHeaderName, moduleHeaderVersion, formatVersionNumbers = None, None, None

        let rec declarations atoms errs =
            let inline setFormatVersion atom fversion =
                match formatVersionNumbers with
                | Some _ -> DuplicateFormatVersion atom :: errs
                | None ->
                    formatVersionNumbers <- Some fversion
                    errs

            // TODO: Avoid code duplication
            let inline setModuleVersion atom mversion =
                match moduleHeaderVersion with
                | Some _ -> DuplicateModuleVersion atom :: errs
                | None ->
                    moduleHeaderVersion <- Some mversion
                    errs

            match atoms with
            // Module format version
            | Atom(Parser.Keyword "format") as fversion :: next ->
                declarations next (setFormatVersion fversion ImmutableArray.Empty)
            | Atom(Parser.NestedAtom(Atom(Parser.Keyword "format") :: numbers)) as fversion :: next ->
                match versionNumberList numbers with
                | Ok numbers' -> declarations next (setFormatVersion fversion numbers')
                | Error err -> declarations next (err :: errs)
            // Module name
            | Atom(Parser.Keyword "name") as mname :: next ->
                declarations next (MissingModuleName mname :: errs)
            | Atom(Parser.NestedAtom(Atom(Parser.Keyword "name") as mname :: contents)) :: next ->
                match contents with
                | Atom(Parser.StringLiteral name) :: extra ->
                    match extra, moduleHeaderName with
                    | [], None ->
                        moduleHeaderName <- Some(Name.ofStr name)
                        declarations next errs
                    | _, Some _ ->
                        declarations next (DuplicateModuleName mname :: errs)
                    | _ :: _, None ->
                        declarations next (extraneous extra errs)
                | _ -> declarations next (MissingModuleName mname :: errs)
            // Module version
            // TODO: Avoid code duplication with other version like nodes
            | Atom(Parser.Keyword "version") as mversion :: next ->
                declarations next (setModuleVersion mversion ImmutableArray.Empty)
            | Atom(Parser.NestedAtom(Atom(Parser.Keyword "version") :: numbers)) as mversion :: next ->
                match versionNumberList numbers with
                | Ok numbers' -> declarations next (setModuleVersion mversion numbers')
                | Error err -> declarations next (err :: errs)

            // ((double nested))
            | Atom(Parser.NestedAtom [ Atom(Parser.NestedAtom _) as nested ]) :: next -> // TODO: Check that this can parse ((double nested)) things.
                declarations (nested :: next) errs
            // Unknown
            | bad :: next ->
                declarations next (UnexpectedAtom bad :: errs)
            | [] -> errs

        match declarations atoms' List.empty with
        | [] ->
            Ok(failwith "TODO: Build the module": Module)
        | errs -> Error errs
    | Error err, _ -> Error [ err ]
