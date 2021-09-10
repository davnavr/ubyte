module UByte.Interpreter.Program

open System.IO

open Argu

type Argument =
    | [<ExactlyOnce>] Program of ``program.mdle``: string
    //| Additional_Import_Dirs
    | Launch_Interpreter_Debugger

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Program _ -> "path to the program to run"
            | Launch_Interpreter_Debugger -> "used to debug the interpreter"

let interpreterArgumentParser = ArgumentParser.Create<Argument>()

[<EntryPoint>]
let main argv =
    // TODO: Use Argu to parse arguments intended for the interpreter
    let iargs, pargs =
        match Array.tryFindIndex ((=) "--") argv with
        | Some i ->
            argv.[0..(i - 1)], argv.[i + 1..]
        | None ->
            argv, Array.empty

    let iargs' = interpreterArgumentParser.Parse(inputs = iargs)

    if iargs'.Contains <@ Launch_Interpreter_Debugger @> then System.Diagnostics.Debugger.Launch() |> ignore

    Runtime.run (FileInfo(iargs'.GetResult <@ Program @>)) Array.empty
