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
            | Import _ -> "makes the types and methods in the specified module available for use during compilation"
            | Input _ -> "specifies a source code file to include as part of the compilation"
            | Module_Name _ -> "specify the name of the compiled module"
            | Module_Version _ -> "specify the version of the compiled module"
            | Output _ -> "specifies the output file containing the compiled binary module"
            //| Parallel -> "specify that parsing of source code should be done in parallel"
            | Launch_Debugger -> "launches the debugger"

let parser = ArgumentParser.Create<Argument>(programName = "middlec")

[<EntryPoint>]
let main argv =
    let args = parser.ParseCommandLine argv

    if args.Contains <@ Launch_Debugger @> then System.Diagnostics.Debugger.Launch() |> ignore

    let outputFilePath = args.TryGetResult <@ Output @> |> Option.defaultWith (fun() -> ".\out.binmdl")

    let result =
        let inputs = ImmutableArray.CreateBuilder()
        let parsedModuleImports = ImmutableArray.CreateBuilder<UByte.Format.Model.Module>()

        List.iter
            (function
            | Argument.Input path ->
                inputs.Add(MiddleC.Compiler.Parser.Parse.fromPath(System.IO.Path.GetFullPath path))
            | Argument.Import path ->
                parsedModuleImports.Add(UByte.Format.ParseModule.fromPath(System.IO.Path.GetFullPath path))
            | _ -> ())
            (args.GetAllResults())

        let resolvedModuleImports = System.Collections.Generic.Dictionary<_, UByte.Resolver.ResolvedModule>()

        let moduleImportResolver id =
            let mutable i, result = 0, ValueNone

            while i < parsedModuleImports.Count && result.IsNone do
                let import = parsedModuleImports.[i]
                if id = import.Header.Module then
                    result <- ValueSome import

            match result with
            | ValueSome import ->
                let iname = import.Header.Module
                let resolved = resolvedModuleImports.[iname]
                resolvedModuleImports.[iname] <- resolved
                resolved
            | ValueNone -> invalidOp(sprintf "Add a reference to the module %O" id)

        for import in parsedModuleImports do
            resolvedModuleImports.Add(import.Header.Module, UByte.Resolver.ResolvedModule(import, moduleImportResolver))

        let defaultModuleName() = System.IO.Path.GetFileNameWithoutExtension outputFilePath

        MiddleC.Compiler.Semantics.TypeChecker.check
            (Option.defaultWith defaultModuleName (args.TryGetResult <@ Module_Name @>))
            (Option.defaultValue List.empty (args.TryGetResult <@ Module_Version @>))
            (inputs.ToImmutable())
            (resolvedModuleImports.Values.ToImmutableArray())

    if result.Errors.IsDefaultOrEmpty then
        UByte.Format.WriteModule.toPath outputFilePath (MiddleC.Compiler.CodeGenerator.write result)
        0
    else
        for error in result.Errors do
            eprintfn "ERR %s(%i,%i): %O" error.Source.Source error.Line error.Column error.Message
        -1
