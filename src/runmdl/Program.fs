module UByte.Interpreter.Program

open System.Collections.Generic
open System.IO

open Argu

open UByte.Runtime

[<System.Runtime.CompilerServices.IsReadOnly; Struct; RequireQualifiedAccess>]
type LogEventTypes =
    | Resolution

type Argument =
    | [<ExactlyOnce>] Program of ``program.binmdl``: string
    | Import_Directory of directory: string
    | [<AltCommandLine("-i")>] Import of ``library.binmdl``: string
    | Launch_Interpreter_Debugger
    | Log_Events of LogEventTypes list
    | Log_To_File of ``log.txt``: string
    | Log_To_Stdout

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Program _ -> "path to the program to run"
            | Launch_Interpreter_Debugger -> "used to debug the interpreter"
            | Import_Directory _ -> "specify a path to a directory containing libraries that the program imports"
            | Import _ -> "specify a path to a library that the program imports"
            | Log_Events _ -> "enables logging for the specified events"
            | Log_To_File _ -> "specify a path to a file that log messages are written to"
            | Log_To_Stdout -> "specify that log messages should also be written to standard output"

let interpreterArgumentParser = ArgumentParser.Create<Argument>()

[<AutoOpen>]
module Logger =
    open UByte.Resolver

    let logn (loggers: List<struct(bool * TextWriter)>) (message: string) =
        let now = System.DateTime.UtcNow
        for struct(_, logger) in loggers do
            logger.Write '['
            logger.Write now
            logger.Write "] "
            logger.WriteLine message

    let logfn loggers format = Printf.kprintf (logn loggers) format

    let setupResolutionLogger loggers =
        let rec inner (rm: ResolvedModule) =
            rm.ModuleResolving.Add <| fun args ->
                logfn loggers "Loading module %O imported by %O" args.Import args.Originator
            rm.TypeResolving.Add <| fun args ->
                logfn loggers "Loading type definition TYPE NAME in %O from %O" args.Owner args.Originator
            rm.MethodResolving.Add <| fun args ->
                logfn loggers "Loading method METHOD NAME in %O from %O" args.Owner args.Originator
            rm.FieldResolving.Add <| fun args ->
                logfn loggers "Loading field FIELD NAME in %O from %O" args.Owner args.Originator
        inner

[<EntryPoint>]
let main argv =
    let iargs, pargs =
        match Array.tryFindIndex ((=) "--") argv with
        | Some i ->
            argv.[0..(i - 1)], argv.[i + 1..]
        | None ->
            argv, Array.empty

    let iargs' = interpreterArgumentParser.Parse(inputs = iargs)
    let fullArgumentList = iargs'.GetAllResults()

    if iargs'.Contains <@ Launch_Interpreter_Debugger @> then System.Diagnostics.Debugger.Launch() |> ignore

    let program = FileInfo(iargs'.GetResult <@ Program @>)

    let importedModuleFiles =
        let files =
            iargs'.GetResults <@ Import @>
            |> Seq.map FileInfo
            |> List

        for dir in iargs'.GetResults <@ Import_Directory @> do
            let dir' = DirectoryInfo dir
            files.AddRange(dir'.GetFiles("*.binmdl"))

        files

    let moduleImportResolver =
        // TODO: Instead of using directory of program, only use explicitly specified directories.
        let imports =
            lazy
                let lookup = Dictionary importedModuleFiles.Count

                for file in importedModuleFiles do
                    if file.FullName <> program.FullName then
                        let parsed = UByte.Format.ParseModule.fromPath file.FullName
                        lookup.Add(parsed.Header.Module, parsed)

                lookup

        fun import ->
            match imports.Value.TryGetValue import with
            | true, existing -> ValueSome existing
            | false, _ -> ValueNone

    let loggers = List<struct(bool * TextWriter)>()

    try
        let logEventCategories = HashSet<LogEventTypes>()

        List.iter
            (function
            | Log_To_File destination ->
                loggers.Add(struct(true, new StreamWriter(destination) :> TextWriter))
            | Log_To_Stdout ->
                loggers.Add(struct(false, stdout))
            | Log_Events types -> for ty in types do logEventCategories.Add ty |> ignore
            | _ -> ())
            fullArgumentList

        use runtime =
            Interpreter.Runtime.Initialize ( // TODO: Use actual constructor so caller knows Runtime is disposable?
                UByte.Format.ParseModule.fromPath program.FullName,
                moduleImportResolver
                // TODO: Allow selection of a GC strategy from command line options
            )

        if logEventCategories.Contains LogEventTypes.Resolution then
            setupResolutionLogger loggers runtime.Program

        runtime.InvokeEntryPoint pargs
    finally
        for struct(dispose, logger) in loggers do
            if dispose && logger <> null then logger.Close()
