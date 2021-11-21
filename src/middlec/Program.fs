[<RequireQualifiedAccess>]
module MiddleC.Compiler.Program

open System.Collections.Immutable

open Argu

[<NoComparison; NoEquality>]
type Argument =
    | [<AltCommandLine("-r")>] Import of ``library.binmdl``: string
    | [<AltCommandLine("-i")>] Input of ``source.mdlc``: string
    | [<Unique>] Module_Name of string
    | [<Unique>] Module_Version of uint32 list
    | [<Unique; AltCommandLine("-o")>] Output of ``output.binmdl``: string
    //| Parallel
    | Launch_Debugger


    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Import _ -> "makes the code "
            | Input _ -> "specifies a source code file to include as part of the compilation"
            | Module_Name _ -> "specify the name of the compiled module"
            | Module_Version _ -> "specify the version of the compiled module"
            | Output _ -> "specifies the output file containing the compiled binary module"
            //| Parallel -> "specify that parsing of source code should be done in parallel"
            | Launch_Debugger -> "launches the debugger"

let parser = ArgumentParser.Create<Argument>(programName = "middlec")

let parseInputFiles (args: ParseResults<Argument>) =
    let results = ImmutableArray.CreateBuilder()
    let rec inner paths =
        match paths with
        | [] -> results.ToImmutable()
        | path :: remaining ->
            let fullFilePath = System.IO.Path.GetFullPath path
            results.Add(MiddleC.Compiler.Parser.Parse.fromPath fullFilePath)
            inner remaining
    inner (args.GetResults <@ Input @>)

[<EntryPoint>]
let main argv =
    let args = parser.ParseCommandLine argv

    if args.Contains <@ Launch_Debugger @> then System.Diagnostics.Debugger.Launch() |> ignore

    if args.Contains <@ Import @> then raise(System.NotImplementedException "Module imports are not yet supported")

    let outputFilePath = args.TryGetResult <@ Output @> |> Option.defaultWith (fun() -> ".\out.binmdl")

    let result =
        let defaultModuleName() = System.IO.Path.GetFileNameWithoutExtension outputFilePath
        MiddleC.Compiler.Semantics.TypeChecker.check
            (Option.defaultWith defaultModuleName (args.TryGetResult <@ Module_Name @>))
            (Option.defaultValue List.empty (args.TryGetResult <@ Module_Version @>))
            (parseInputFiles args)
            ImmutableArray.Empty

    if result.Errors.IsDefaultOrEmpty then
        UByte.Format.WriteModule.toPath outputFilePath (MiddleC.Compiler.CodeGenerator.write result)
        0
    else
        for error in result.Errors do
            eprintfn "ERR %s(%i,%i): %O" error.Source.Source error.Line error.Column error.Message
        -1
