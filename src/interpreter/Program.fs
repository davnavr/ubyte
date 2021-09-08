module UByte.Interpreter.Program

open System.IO

open Argu

type Argument =
    | [<ExactlyOnce>] Program of ``program.mdle``: string
    //| OptionForAdditionalProbingPaths

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Program _ -> "path to the program to run"

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

    let runtime = Runtime.State(FileInfo(iargs'.GetResult <@ Program @>))

    0 // return an integer exit code
