module UByte.Interpreter.Program

open System.Collections.Generic
open System.Diagnostics
open System.IO

open Argu

open UByte.Runtime

[<Struct; RequireQualifiedAccess>]
type LogEventTypes =
    | Resolution
    | Allocation

[<Struct; RequireQualifiedAccess>]
type GarbageCollectionStrategy =
    | Mark_And_Sweep
    | Mark_And_Compact

type Argument =
    | [<ExactlyOnce>] Program of ``program.binmdl``: string
    | Import_Directory of directory: string
    | [<AltCommandLine("-i")>] Import of ``library.binmdl``: string
    | Launch_Interpreter_Debugger
    | Log_Events of LogEventTypes list
    | Log_To_File of ``log.txt``: string
    | Log_To_Stdout
    | [<Unique>] Trace of ``trace.speedscope.json``: string option
    | [<Unique>] Garbage_Collector of GarbageCollectionStrategy

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
            | Trace _ -> "specify a path to a file that will contain profiling information in speedscope format, defaults to a file named after the program in the current directory"
            | Garbage_Collector _ -> "specify the garbage collector to use"

let [<Literal>] InterpreterProgramName = "runmdl"

let interpreterArgumentParser = ArgumentParser.Create<Argument>(programName = InterpreterProgramName)

[<AutoOpen>]
module Logger =
    open UByte.Resolver

    let logn (loggers: List<struct(bool * TextWriter)>) (message: string) =
        if loggers.Count > 0 then
            let now = System.DateTime.UtcNow.ToString("s")
            for struct(_, logger) in loggers do
                logger.Write '['
                logger.Write now
                logger.Write "] "
                logger.WriteLine message

    let logfn loggers format = Printf.kprintf (logn loggers) format

    let setupResolutionLogger loggers =
        let rec inner (rm: ResolvedModule) =
            rm.ModuleResolving.Add <| fun args ->
                logfn loggers "Resolving module %O referenced by %O" args.Import args.Originator
            rm.ModuleResolved.Add <| fun args ->
                logfn loggers "Successfully resolved module %O imported by %O" args.Import args.Originator
            rm.TypeResolving.Add <| fun args ->
                logfn loggers "Resolving type definition TYPE NAME in %O from %O" args.Owner args.Originator
            rm.TypeResolved.Add <| fun ty ->
                logfn loggers "Successfully resolved type definition %O" ty
            rm.MethodResolving.Add <| fun args ->
                logfn loggers "Resolving method METHOD NAME in %O from %O" args.Owner args.Originator
            rm.MethodResolved.Add <| fun method ->
                logfn loggers "Successfully resolved method %O" method
            rm.FieldResolving.Add <| fun args ->
                logfn loggers "Resolving field FIELD NAME in %O from %O" args.Owner args.Originator
            rm.FieldResolved.Add <| fun field ->
                logfn loggers "Successfully resolved field %O" field
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

    if iargs'.Contains <@ Launch_Interpreter_Debugger @> then Debugger.Launch() |> ignore

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
        use gc =
            match iargs'.TryGetResult <@ Garbage_Collector @> with
            | None
            | Some GarbageCollectionStrategy.Mark_And_Sweep ->
                MemoryManagement.GarbageCollectors.MarkAndSweep()
            | Some GarbageCollectionStrategy.Mark_And_Compact ->
                MemoryManagement.GarbageCollectors.MarkAndCompact()

        use runtime =
            Interpreter.Runtime.Initialize ( // TODO: Use actual constructor so caller knows Runtime is disposable?
                program = UByte.Format.ParseModule.fromPath program.FullName,
                moduleImportLoader = moduleImportResolver,
                garbageCollectorStrategy = gc
            )

        let logEventCategories = HashSet<LogEventTypes>()
        let mutable interpreterTraceHandler, interpreterTraceOutput = None, None
        let timer = ref None

        List.iter
            (function
            | Log_To_File destination ->
                loggers.Add(struct(true, new StreamWriter(destination) :> TextWriter))
            | Log_To_Stdout ->
                loggers.Add(struct(false, stdout))
            | Log_Events types -> for ty in types do logEventCategories.Add ty |> ignore
            | Trace destination ->
                let destination' =
                    Path.ChangeExtension(program.FullName, ".speedscope.json")
                    |> defaultArg destination
                    |> FileInfo

                let events = System.Collections.Immutable.ImmutableArray.CreateBuilder<Speedscope.FrameEvent>()

                interpreterTraceOutput <- Some(destination', events)

                interpreterTraceHandler <- Some <| fun (source: Interpreter.EventSource) ->
                    timer.Value <- Some(Stopwatch.StartNew())
                    let timer = timer.Value.Value

                    events.Add
                        { Speedscope.Time = System.TimeSpan.Zero
                          Speedscope.Type = Speedscope.OpenFrame
                          Speedscope.Name = InterpreterProgramName }

                    source.MethodCalled.Add <| fun frame ->
                        let time = timer.Elapsed
                        events.Add
                            { Speedscope.Time = time
                              Speedscope.Type = Speedscope.OpenFrame
                              Speedscope.Name = frame.CurrentMethod.ToString() }

                    source.MethodReturned.Add <| fun frame ->
                        let time = timer.Elapsed
                        events.Add
                            { Speedscope.Time = time
                              Speedscope.Type = Speedscope.CloseFrame
                              Speedscope.Name = frame.CurrentMethod.ToString() }
            | _ -> ())
            fullArgumentList

        if logEventCategories.Contains LogEventTypes.Resolution then
            setupResolutionLogger loggers runtime.Program

        let stackEventHandler =
            if logEventCategories.Contains LogEventTypes.Allocation then
                gc.Allocated.Add <| fun(struct(size, addr)) ->
                    logfn loggers "Allocated object at %O (%i bytes)" addr size

                gc.CollectionStarted.Add <| fun() ->
                    logfn loggers "Garbage collection started"

                gc.Collected.Add <| fun addr ->
                    logfn loggers "Collected object at 0x%08X" addr

                gc.CollectionEnded.Add <| fun count ->
                    logfn loggers "Garbage collection ended, freed %i objects" count

                gc.Moved.Add <| fun struct(old, current) ->
                    logfn loggers "Relocated object from 0%08X to %O" old current

                Some <| fun (stack: MemoryManagement.ValueStack) ->
                    stack.Allocated.Add <| fun arg ->
                        logfn loggers "Allocated %i bytes on stack at 0x%08X" arg.Size arg.Address
                    stack.Freed.Add <| fun arg ->
                        logfn loggers "Freed %i bytes from stack" arg
            else
                None

        let result =
            runtime.InvokeEntryPoint (
                argv = pargs,
                ?interpreterEventHandler = interpreterTraceHandler,
                ?stackEventHandler = stackEventHandler
            )

        if timer.Value.IsSome then
            let timer = timer.Value.Value
            timer.Stop()

            match interpreterTraceOutput with
            | Some(destination, events) ->
                events.Add
                    { Speedscope.Time = timer.Elapsed
                      Speedscope.Type = Speedscope.CloseFrame
                      Speedscope.Name = InterpreterProgramName }

                use output = destination.Open(FileMode.Create)
                Speedscope.writeToStream output (events.ToImmutable()) (System.TimeSpan.Zero) timer.Elapsed program.Name
            | None -> ()

        result
    finally
        for struct(dispose, logger) in loggers do
            if dispose && logger <> null then logger.Close()
