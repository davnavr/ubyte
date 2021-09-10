module UByte.Format.WriteModule

open System.IO

open UByte.Format.Model

let inline bits1 (value: 'Enum when 'Enum : enum<uint8>) (dest: Stream) = dest.WriteByte(uint8 value)

[<RequireQualifiedAccess>]
module LEB128 =
    let [<Literal>] private ContinueMask = 0b1000_0000uy

    let inline private unsigned value (dest: Stream) =
        if value <> LanguagePrimitives.GenericZero then
            let mutable value = value
            while value > LanguagePrimitives.GenericZero do
                let b = uint8 value &&& (~~~ContinueMask)
                value <- value >>> 7;
                let cont = if value > LanguagePrimitives.GenericZero then ContinueMask else 0uy
                dest.WriteByte(b ||| cont)
        else
            dest.WriteByte 0uy

    let uint value dest = unsigned value dest

let vector writer (items: vector<'T>) dest =
    LEB128.uint (uint32 items.Length) dest
    for i = 0 to items.Length - 1 do
        writer items.[i] dest

let versions (VersionNumbers numbers) dest = vector LEB128.uint numbers dest

let name (Name name) dest =
    failwith "TODO: Write name"

let lengthEncodedData (buffer: MemoryStream) dest writer =
    let inline reset() = buffer.Seek(0L, SeekOrigin.Begin) |> ignore

    buffer.SetLength 0L
    reset()
    writer buffer

    LEB128.uint (uint32 buffer.Length) dest
    buffer.WriteTo dest

let moduleID id dest =
    name id.ModuleName dest
    versions id.Version dest

let toStream (stream: Stream) (md: Module) =
    if isNull stream then nullArg(nameof stream)
    if not stream.CanWrite then invalidArg (nameof stream) "The stream must support writing"
    try
        let buffer = new MemoryStream()
        let (Magic magic) = md.Magic
        stream.Write(magic.AsSpan())
        versions md.FormatVersion stream

        lengthEncodedData buffer stream <| fun dest ->
            let header = md.Header
            LEB128.uint header.FieldCount dest
            moduleID header.Module dest
            bits1 header.Flags dest
            bits1 header.PointerSize dest


    finally
        stream.Close()

let toFile (destination: FileInfo) md =
    if isNull destination then nullArg(nameof destination)
    toStream (destination.OpenWrite()) md

let toPath destination md = toFile (FileInfo destination) md
