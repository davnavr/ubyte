module UByte.Format.ParseModule

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.CompilerServices

[<Interface>]
type IByteSequence = abstract Read: buffer: Span<byte> -> int32

[<Interface>]
type IParser<'Result> = abstract Parse: source: #IByteSequence -> 'Result

let inline parse<'Parser, 'Result, 'Source
    when 'Parser :> IParser<'Result>
    and 'Parser : struct
    and 'Source :> IByteSequence>
    (source: 'Source)
    =
    Unchecked.defaultof<'Parser>.Parse source

[<RequireQualifiedAccess>]
module LEB128 =
    let [<Literal>] private ContinueMask = 0b1000_0000uy

    let inline private unsigned size convert (stream: #IByteSequence) =
        let mutable cont, n, shifted = true, LanguagePrimitives.GenericZero, 0
        while cont do
            let b = readByte stream
            cont <- b &&& ContinueMask = ContinueMask
            n <- n ||| (convert (b &&& (~~~ContinueMask)) <<< shifted)

            let shifted' =
                if cont then 7
                elif b >= 64uy then 6
                elif b >= 32uy then 5
                elif b >= 16uy then 4
                elif b >= 8uy then 3
                elif b >= 4uy then 2
                elif b >= 2uy then 1
                else 0

            shifted <- Checked.(+) shifted shifted'
            if shifted > size then failwith "TODO: Error for exceeded max allowed value for this kind of LEB128 integer"
        n

    [<Struct; NoComparison; NoEquality>]
    type UInt =
        interface IParser<Format.uvarint> with member _.Parse source = unsigned 32 uint32 source

[<IsReadOnly; Struct>]
type StreamWrapper (stream: Stream) =
    interface IByteSequence with
        member _.Read buffer = stream.Read buffer

// TODO: Have base class for exceptions.
let magic (source: #IByteSequence) =
    let magic' = Format.magic.Magic
    let buffer = Span.stackalloc magic'.Length
    if source.Read buffer = 4 && Equality.spans (Span.asReadOnly buffer) (magic'.AsSpan()) then
        Format.magic
    else failwithf "TODO: Error for invalid magic"

let vector<'Parser, 'Result, 'Source
    when 'Parser :> IParser<'Result>
    and 'Parser : struct
    and 'Source :> IByteSequence>
    (source: 'Source)
    =
    let mutable items =
        let length = Checked.int32(parse<LEB128.UInt, _, _> source)
        Array.zeroCreate<'Result> length

    for i = 0 to items.Length - 1 do
        items.[i] <- parse<'Parser, 'Result, _> source

    Unsafe.As<_, ImmutableArray<'Result>> &items

let versions source = Format.VersionNumbers(vector<LEB128.UInt, _, _> source)

let lengthEncodedData (source: #IByteSequence) =
    let data =
        parse<LEB128.UInt, _, _> source
        |> Checked.int32
        |> Array.zeroCreate<byte>
    let read = source.Read(Span(data, 0, data.Length))
    if read <> data.Length then failwithf "TODO: Unexpected end of data of length %i" data.Length
    StreamWrapper(new MemoryStream(data))

let header source =
    let header' = lengthEncodedData source
    let fcount = parse<LEB128.UInt, _, _> source
    if fcount <> 3u then failwithf "TODO: Invalid field count %i" fcount
    
    {  }

let fromBytes (source: #IByteSequence) =
    let magic' = magic source
    let fversion = versions source
    if fversion <> Format.currentFormatVersion then failwithf "TODO: Error for unsupported version %O" fversion
    
    ()

let fromStream (source: Stream) =
    if isNull source then nullArg(nameof source)
    try
        fromBytes(StreamWrapper source)
    finally
        source.Close()
