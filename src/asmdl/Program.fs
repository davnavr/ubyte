module UByte.Assembler.Program

open System.IO

open Argu

[<NoComparison; NoEquality>]
type Argument =
    | [<ExactlyOnce>] Input of ``source.txtmdl``: string
    | [<Unique; AltCommandLine("-o")>] Output of ``program.binmdl``: string
    | Launch_Debugger

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Input _ -> "specifies the input file containing the text module to assemble"
            | Output _ -> "specifies the output file containing the assembled binary module"
            | Launch_Debugger -> "launches the debugger"

let parser = ArgumentParser.Create<Argument>()

[<EntryPoint>]
let main argv =
    let args = parser.ParseCommandLine argv

    if args.Contains <@ Launch_Debugger @> then System.Diagnostics.Debugger.Launch() |> ignore

    let input = FileInfo(args.GetResult <@ Input @>)

    let output =
        args.TryGetResult <@ Output @>
        |> Option.defaultWith (fun() -> Path.ChangeExtension(input.FullName, "binmdl"))
        |> FileInfo

    use input' = input.OpenRead()

    let result =
        Assemble.fromInput <| fun parser state ->
            FParsec.CharParsers.runParserOnStream parser state input.Name input' System.Text.Encoding.UTF8

    match result with
    | Ok mdle ->
        output.Directory.Create()
        use output' = output.Create()
        UByte.Format.WriteModule.toStream output' mdle
        0
    | Error errors ->
        for err in errors do stderr.WriteLine err
        -1
