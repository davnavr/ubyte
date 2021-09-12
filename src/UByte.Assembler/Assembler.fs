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

//type AssemblerBuilder () =
//    member inline _.Bind(expr: Assembler<'T>, body: 'T -> Assembler<'U>): Assembler<'U> =
//        fun atoms errs ->
//            match expr atoms errs with
//            | Ok(t, errs'), atoms' -> body t atoms' errs'
//            | Error errs', atoms' -> Error errs', atoms'

//let assembler = AssemblerBuilder()

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
                inner (struct(Error errs', atoms')) remaining atoms errors state

    fun atoms -> inner (struct(Error List.empty, atoms)) parsers atoms

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
            | Ok(_, errors'), extra -> Error(errorExtraAtoms extra errors' @ errors), atoms'
            | Error errors', _ -> Error(errors' @ errors), atoms'
        | _ -> failwith "TODO: How to deal with this?"

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

//let keyword name atoms: struct(_ * _) =
//    match atoms with
//    | [] -> Error(ExpectedKeyword(name, None)), atoms
//    | Atom(Parser.Keyword actual) :: rest when StringComparer.Ordinal.Equals(name, actual) -> Ok(), rest
//    | bad :: rest -> Error(ExpectedKeyword(name, Some bad)), rest

//let versionNumberList atoms =
//    let rec inner (numbers: ImmutableArray<_>.Builder) atoms =
//        match atoms with
//        | [] ->
//            Ok(numbers.ToImmutable())
//        | Atom(Parser.IntegerLiteral n) :: numbers' when n >= 0L ->
//            numbers.Add(uint32 n)
//            inner numbers numbers'
//        | bad :: _ ->
//            Error(InvalidVersionNumber bad)
//    inner (ImmutableArray.CreateBuilder()) atoms

let versionDirective name = directive name (fun atom -> Ok(atom, VersionNumbers.empty)) (fun atom -> ret atom .>>. versionNumberList)

let moduleVersionDirective name (getVersionField: _ -> _ ref) err: Assembler<_> =
    versionDirective name
    .>>. tryUpdateState (getVersionField >> Ok)
    |> tryMap (fun ((atom, version), update) ->
        match update.contents with
        | ValueNone ->
            update.contents <- ValueSome version
            Ok()
        | ValueSome _ -> Error(err atom))

let assemblerEntryPoint: Assembler<unit> =
    keyword "module"
    >>. choice [
            moduleVersionDirective "format" (fun state -> state.ModuleFormatVersion) DuplicateFormatVersion

            moduleVersionDirective "version" (fun state -> state.ModuleFormatVersion) DuplicateModuleVersion
        ]

let assemble atoms =
    let state =
        { ModuleFormatVersion = ref ValueNone
          ModuleHeaderName = ValueNone
          ModuleHeaderVersion = ref ValueNone }

    match assemblerEntryPoint atoms List.empty state with
    | Ok _, [] ->
        Ok(failwith "bad": Module)
    | result, extra ->
        match result with
        | Ok _ -> List.empty
        | Error errs -> errs
        |> errorExtraAtoms extra
        |> Error

    //match keyword "module" atoms with
    //| Ok(), atoms' ->
    //    let mutable moduleHeaderName, moduleHeaderVersion, formatVersionNumbers = None, None, None

    //    let rec declarations atoms errs =
    //        let inline setFormatVersion atom fversion =
    //            match formatVersionNumbers with
    //            | Some _ -> DuplicateFormatVersion atom :: errs
    //            | None ->
    //                formatVersionNumbers <- Some fversion
    //                errs

    //        // TODO: Avoid code duplication
    //        let inline setModuleVersion atom mversion =
    //            match moduleHeaderVersion with
    //            | Some _ -> DuplicateModuleVersion atom :: errs
    //            | None ->
    //                moduleHeaderVersion <- Some mversion
    //                errs

    //        match atoms with
    //        // Module format version
    //        | Atom(Parser.Keyword "format") as fversion :: next ->
    //            declarations next (setFormatVersion fversion ImmutableArray.Empty)
    //        | Atom(Parser.NestedAtom(Atom(Parser.Keyword "format") :: numbers)) as fversion :: next ->
    //            match versionNumberList numbers with
    //            | Ok numbers' -> declarations next (setFormatVersion fversion numbers')
    //            | Error err -> declarations next (err :: errs)
    //        // Module name
    //        | Atom(Parser.Keyword "name") as mname :: next ->
    //            declarations next (MissingModuleName mname :: errs)
    //        | Atom(Parser.NestedAtom(Atom(Parser.Keyword "name") as mname :: contents)) :: next ->
    //            match contents with
    //            | Atom(Parser.StringLiteral name) :: extra ->
    //                match extra, moduleHeaderName with
    //                | [], None ->
    //                    moduleHeaderName <- Some(Name.ofStr name)
    //                    declarations next errs
    //                | _, Some _ ->
    //                    declarations next (DuplicateModuleName mname :: errs)
    //                | _ :: _, None ->
    //                    declarations next (extraneous extra errs)
    //            | _ -> declarations next (MissingModuleName mname :: errs)
    //        // Module version
    //        // TODO: Avoid code duplication with other version like nodes
    //        | Atom(Parser.Keyword "version") as mversion :: next ->
    //            declarations next (setModuleVersion mversion ImmutableArray.Empty)
    //        | Atom(Parser.NestedAtom(Atom(Parser.Keyword "version") :: numbers)) as mversion :: next ->
    //            match versionNumberList numbers with
    //            | Ok numbers' -> declarations next (setModuleVersion mversion numbers')
    //            | Error err -> declarations next (err :: errs)

    //        // ((double nested))
    //        | Atom(Parser.NestedAtom [ Atom(Parser.NestedAtom _) as nested ]) :: next -> // TODO: Check that this can parse ((double nested)) things.
    //            declarations (nested :: next) errs
    //        // Unknown
    //        | bad :: next ->
    //            declarations next (UnexpectedAtom bad :: errs)
    //        | [] -> errs

    //    match declarations atoms' List.empty with
    //    | [] ->
    //        Ok(failwith "TODO: Build the module": Module)
    //    | errs -> Error errs
    //| Error err, _ -> Error [ err ]
