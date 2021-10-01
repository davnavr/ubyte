module UByte.Interpreter.Program

open System.IO

open Argu

type Argument =
    | [<ExactlyOnce>] Program of ``program.binmdl``: string
    //| Import_Dir
    //| Import
    | Launch_Interpreter_Debugger

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Program _ -> "path to the program to run"
            | Launch_Interpreter_Debugger -> "used to debug the interpreter"

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

    let programFilePath = FileInfo(iargs'.GetResult <@ Program @>)

    let moduleImportResolver =
        // TODO: Instead of using directory of program, only use explicitly specified directories.
        let imports =
            lazy
                let modules = programFilePath.Directory.GetFiles("*.binmdl")
                let lookup = System.Collections.Generic.Dictionary modules.Length

                for file in modules do
                    if file.FullName <> programFilePath.FullName then
                        let parsed = UByte.Format.ParseModule.fromPath file.FullName
                        lookup.Add(parsed.Header.Module, parsed)

                lookup

        fun import ->
            match imports.Value.TryGetValue import with
            | true, existing -> ValueSome existing
            | false, _ -> ValueNone

    let runtime =
        Runtime.initialize (UByte.Format.ParseModule.fromPath programFilePath.FullName) moduleImportResolver

    runtime.InvokeEntryPoint pargs
