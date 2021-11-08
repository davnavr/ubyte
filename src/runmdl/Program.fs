module UByte.Interpreter.Program

open System.IO

open Argu

type Argument =
    | [<ExactlyOnce>] Program of ``program.binmdl``: string
    | Import_Directory of directory: string
    | [<AltCommandLine("-i")>] Import of ``library.binmdl``: string
    | Launch_Interpreter_Debugger

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Program _ -> "path to the program to run"
            | Launch_Interpreter_Debugger -> "used to debug the interpreter"
            | Import_Directory _ -> "specifies a path to a directory containing libraries that the program imports"
            | Import _ -> "specifies a path to a library that the program imports"

let interpreterArgumentParser = ArgumentParser.Create<Argument>()

[<EntryPoint>]
let main argv =
    let iargs, pargs =
        match Array.tryFindIndex ((=) "--") argv with
        | Some i ->
            argv.[0..(i - 1)], argv.[i + 1..]
        | None ->
            argv, Array.empty

    let iargs' = interpreterArgumentParser.Parse(inputs = iargs)

    if iargs'.Contains <@ Launch_Interpreter_Debugger @> then System.Diagnostics.Debugger.Launch() |> ignore

    let program = FileInfo(iargs'.GetResult <@ Program @>)

    let importedModuleFiles =
        let files =
            iargs'.GetResults <@ Import @>
            |> Seq.map FileInfo
            |> System.Collections.Generic.List

        for dir in iargs'.GetResults <@ Import_Directory @> do
            let dir' = DirectoryInfo dir
            files.AddRange(dir'.GetFiles("*.binmdl"))

        files

    let moduleImportResolver =
        // TODO: Instead of using directory of program, only use explicitly specified directories.
        let imports =
            lazy
                let lookup = System.Collections.Generic.Dictionary importedModuleFiles.Count

                for file in importedModuleFiles do
                    if file.FullName <> program.FullName then
                        let parsed = UByte.Format.ParseModule.fromPath file.FullName
                        lookup.Add(parsed.Header.Module, parsed)

                lookup

        fun import ->
            match imports.Value.TryGetValue import with
            | true, existing -> ValueSome existing
            | false, _ -> ValueNone

    let runtime =
        UByte.Runtime.Interpreter.Runtime.Initialize (
            UByte.Format.ParseModule.fromPath program.FullName,
            moduleImportResolver
            // TODO: Allow selection of a GC strategy from command line options
        )

    runtime.InvokeEntryPoint pargs
