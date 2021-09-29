module UByte.Format.ParseModule

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

open UByte.Format.Model
open UByte.Format.Model.InstructionSet

[<Interface>]
type IByteSequence = abstract Read: buffer: Span<byte> -> int32

let u1 (stream: #IByteSequence) =
    let buffer = Span.stackalloc<byte> 1
    if stream.Read buffer = 1
    then buffer.[0]
    else failwith "TODO: EOF unexpectedly reached"

let inline bits1<'Enum, 'Source when 'Enum : enum<uint8> and 'Source :> IByteSequence> (stream: 'Source) =
    LanguagePrimitives.EnumOfValue<_, 'Enum>(u1 stream)

[<RequireQualifiedAccess>]
module LEB128 =
    let [<Literal>] private ContinueMask = 0b1000_0000uy

    let inline private unsigned size convert (stream: #IByteSequence) =
        let mutable cont, n, shifted = true, LanguagePrimitives.GenericZero, 0
        while cont do
            let b = u1 stream
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

    let uint (source: #_) = unsigned 32 uint32 source

[<IsReadOnly; Struct>]
type StreamWrapper (stream: Stream) =
    interface IByteSequence with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.Read buffer = stream.Read buffer

// TODO: Have base class for exceptions.
let magic (source: #IByteSequence) =
    let magic' = Model.magic.Magic
    let buffer = Span.stackalloc magic'.Length
    if source.Read buffer = magic'.Length && Equality.spans (Span.asReadOnly buffer) (magic'.AsSpan()) then
        Model.magic
    else failwithf "TODO: Error for invalid magic"

let vector (source: #IByteSequence) parse =
    let mutable items =
        let length = Checked.int32(LEB128.uint source)
        Array.zeroCreate<'Result> length

    for i = 0 to items.Length - 1 do
        items.[i] <- parse source

    Unsafe.As<_, ImmutableArray<'Result>> &items

let inline versions (source: #_) = VersionNumbers(vector source LEB128.uint)

let inline index (source: #_) = Index(LEB128.uint source)

let lengthEncodedData (source: #IByteSequence) =
    let mutable data =
        LEB128.uint source
        |> Checked.int32
        |> Array.zeroCreate<byte>
    let read = source.Read(Span(data, 0, data.Length))
    if read <> data.Length then failwithf "TODO: Unexpected end of data of length %i" data.Length
    struct(Unsafe.As<_, ImmutableArray<byte>> &data, StreamWrapper(new MemoryStream(data)))

let lengthEncodedVector source parse =
    let struct(_, data) = lengthEncodedData source
    vector data parse

let ustring (source: #_) =
    let struct(bytes, _) = lengthEncodedData source
    System.Text.Encoding.UTF8.GetString(bytes.AsSpan())

let name (source: #_) =
    match Name.tryOfStr(ustring source) with
    | ValueSome n -> n
    | ValueNone -> failwithf "TODO: Error for name cannot be empty"

let moduleID (source: #_) =
    { ModuleIdentifier.ModuleName = name source
      Version = versions source }

let header (source: #_) =
    let struct(_, header') = lengthEncodedData source
    let fcount = LEB128.uint header'
    if fcount <> 3u then failwithf "TODO: Invalid field count %i" fcount

    { Module = moduleID header'
      Flags = bits1 header'
      PointerSize =
        let psize = bits1 header'
        if psize > PointerSize.Is64Bit then failwithf "TODO: Invalid pointer size 0x%02X (%A)" (uint8 psize) psize
        psize }

let moduleImports source =
    { ModuleImports.ImportedModules = vector source moduleID
      ImportedTypes =
        lengthEncodedVector source <| fun t ->
            { TypeDefinitionImport.Module = index t
              TypeName = index t
              TypeKind =
                let kind = bits1 t
                if kind > Tag.TypeDefinitionKind.Struct then failwithf "TODO: Invalid type kind 0x%02X (%A)" (uint8 kind) kind
                kind
              TypeParameters = LEB128.uint t }
      ImportedFields =
        lengthEncodedVector source <| fun f ->
            { FieldImport.FieldOwner = index f
              FieldName = index f
              FieldType = index f }
      ImportedMethods =
        lengthEncodedVector source <| fun m ->
            { MethodImport.MethodOwner = index m
              MethodName = index m
              TypeParameters = LEB128.uint m
              Signature = index m } }

let annotation _ = failwith "TODO: Annotations not yet supported"

let genericTypeParam _ = failwith "TODO: Type parameters not yet supported"

let moduleDefinitions source =
    { ModuleDefinitions.DefinedTypes =
        lengthEncodedVector source <| fun t ->
            { TypeDefinition.TypeName = index t
              TypeNamespace = index t
              TypeVisibility = bits1 t
              TypeKind =
                match bits1 t with
                | Tag.TypeDefinitionKind.Class -> Class(ValueSome(index t), bits1 t)
                | Tag.TypeDefinitionKind.Interface -> Interface
                | Tag.TypeDefinitionKind.Struct -> Struct
                | Tag.TypeDefinitionKind.BaseClass -> Class(ValueNone, bits1 t)
                | bad -> failwithf "TODO: Bad type definition kind 0x%02X" (uint8 bad)
              TypeLayout =
                match bits1 t with
                | Tag.TypeDefinitionLayout.Unspecified -> TypeDefinitionLayout.Unspecified
                | Tag.TypeDefinitionLayout.Sequential -> TypeDefinitionLayout.Sequential
                | bad -> failwithf "TODO: Bad type definition layout kind 0x%02X" (uint8 bad)
              ImplementedInterfaces = vector t (fun _ -> failwith "TODO: Interfaces not yet supported")
              TypeParameters = vector t genericTypeParam
              TypeAnnotations = vector t annotation
              Fields = vector t index
              Methods = vector t index }
      DefinedFields =
        lengthEncodedVector source <| fun f ->
            { Field.FieldOwner = index f
              FieldName = index f
              FieldVisibility = bits1 f
              FieldFlags = bits1 f
              FieldType = index f
              FieldAnnotations = vector f annotation }
      DefinedMethods =
        lengthEncodedVector source <| fun m ->
            { Method.MethodOwner = index m
              MethodName = index m
              MethodVisibility = bits1 m
              MethodFlags = bits1 m
              TypeParameters = vector m genericTypeParam
              Signature = index m
              MethodAnnotations = vector m annotation
              Body =
                match bits1 m with
                | Tag.MethodBody.Defined -> MethodBody.Defined(index m)
                | Tag.MethodBody.Abstract -> MethodBody.Abstract
                | bad -> failwithf "TODO: Bad method body kind 0x%02X" (uint8 bad) } }

[<RequireQualifiedAccess>]
module Constant =
    let private integer<'T, 'Source
        when 'T : struct and 'T : (new: unit -> 'T)
        and 'T :> System.ValueType
        and 'Source :> IByteSequence>
        endianness
        (source: 'Source)
        =
        let buffer = Span.stackalloc<byte> sizeof<'T>
        if source.Read buffer <> buffer.Length then failwith "TODO: Error for EOF while parsing integer"

        match endianness with
        | LittleEndian when BitConverter.IsLittleEndian -> ()
        | BigEndian when not BitConverter.IsLittleEndian -> ()
        | _ -> buffer.Reverse()

        MemoryMarshal.Read<'T>(Span.asReadOnly buffer)

    let i32 endianness source = integer<int32, _> endianness source

let instruction endianness source =
    let inline callargs instr: Instruction = instr(index source, vector source index, vector source index)
    let inline index1 instr: Instruction = instr(index source)
    let inline index2 instr: Instruction = instr(index source, index source)
    let inline index3 instr: Instruction = instr(index source, index source, index source)

    match LanguagePrimitives.EnumOfValue(LEB128.uint source) with
    | Opcode.nop -> Nop
    | Opcode.ret -> Ret(vector source index)

    | Opcode.call -> callargs Call
    | Opcode.``call.virt`` -> callargs Call_virt

    // Register
    | Opcode.``reg.copy`` -> index2 Reg_copy

    // Arithmetic
    | Opcode.add -> index3 Add
    | Opcode.sub -> index3 Sub
    | Opcode.mul -> index3 Mul
    | Opcode.div -> index3 Div

    | Opcode.incr -> index1 Incr

    | Opcode.decr -> index1 Decr

    | Opcode.``const.i32`` -> Const_i32(Constant.i32 endianness source, index source)
    
    | Opcode.``const.true`` -> index1 Const_true
    | Opcode.``const.zero`` (*| Opcode.``const.false``*) -> index1 Const_zero

    | Opcode.``and`` -> index3 And
    | Opcode.``or`` -> index3 Or
    | Opcode.``not`` -> index3 Not
    | Opcode.xor -> index3 Xor
    | Opcode.rem -> index3 Rem

    | Opcode.rotl -> index2 Rotl
    | Opcode.rotr -> index2 Rotr

    | Opcode.``obj.null`` -> index1 Obj_null

    | Opcode.``call.ret`` -> callargs Call_ret
    | Opcode.``call.virt.ret`` -> callargs Call_virt_ret

    | bad -> failwithf "TODO: Unrecognized opcode 0x08%X" (uint32 bad)

let fromBytes (source: #IByteSequence) =
    let magic' = magic source
    let fversion = versions source
    if fversion <> Model.currentFormatVersion then failwithf "TODO: Error for unsupported version %O" fversion
    let dcount = LEB128.uint source // TODO: Ensure data vector count is valid.
    if dcount <> currentDataVectorCount then failwithf "TODO: Error for unsupported data vector count %i" dcount
    let header' = header source

    // TODO: Check that length encoded data has no remaining bytes left
    { Module.Magic = magic'
      FormatVersion = fversion
      Header = header'
      Identifiers = { IdentifierSection.Identifiers = lengthEncodedVector source ustring }
      Namespaces = lengthEncodedVector source (fun ns -> vector ns index)
      TypeSignatures =
        lengthEncodedVector source <| fun tsig ->
            match bits1 tsig with
            | Tag.Type.Unit -> Primitive PrimitiveType.Unit
            | Tag.Type.S8 -> Primitive PrimitiveType.S8
            | Tag.Type.S16 -> Primitive PrimitiveType.S16
            | Tag.Type.S32 -> Primitive PrimitiveType.S32
            | Tag.Type.S64 -> Primitive PrimitiveType.S64
            | Tag.Type.U8 -> Primitive PrimitiveType.U8
            | Tag.Type.U16 -> Primitive PrimitiveType.U16
            | Tag.Type.U32 -> Primitive PrimitiveType.U32
            | Tag.Type.U64 -> Primitive PrimitiveType.U64
            | Tag.Type.F32 -> Primitive PrimitiveType.F32
            | Tag.Type.F64 -> Primitive PrimitiveType.F64
            | Tag.Type.Bool -> Primitive PrimitiveType.Bool
            | Tag.Type.Char16 -> Primitive PrimitiveType.Char16
            | Tag.Type.Char32 -> Primitive PrimitiveType.Char32
            | Tag.Type.RefAny -> ObjectReference ReferenceType.Any
            | Tag.Type.RefDefinedType -> ObjectReference(ReferenceType.Defined(index tsig))
            | bad -> failwithf "TODO: Invalid type (0x%02X) %A" (uint8 bad) bad
      MethodSignatures =
        lengthEncodedVector source <| fun msig ->
            { ReturnTypes = vector msig index
              MethodSignature.ParameterTypes = vector msig index }
      Imports =
        let struct(_, imports) = lengthEncodedData source
        moduleImports imports
      Definitions =
        let struct(_, definitions) = lengthEncodedData source
        moduleDefinitions definitions
      Data =
        let struct(_, data) = lengthEncodedData source
        vector data <| fun source ->
            let struct(bytes, _) = lengthEncodedData source
            bytes
      Code =
        lengthEncodedVector source  <| fun code ->
            { Code.RegisterTypes =
                vector code <| fun body ->
                    let count = LEB128.uint body
                    let rtype =
                        { RegisterType = index body
                          RegisterFlags = bits1 body }
                    struct(count, rtype)
              Instructions =
                let struct(_, instructions) = lengthEncodedData code
                vector instructions (instruction header'.Endianness) }
      EntryPoint =
        let struct(epoint, epoint') = lengthEncodedData source
        if epoint.IsDefaultOrEmpty
        then ValueNone
        else ValueSome(index epoint')
      Debug =
        let struct(debug, _) = lengthEncodedData source
        if debug.Length > 0 then failwith "TODO: Debugging information not yet supported"
        () }

let fromStream (source: Stream) =
    if isNull source then nullArg(nameof source)
    if not source.CanRead then invalidArg (nameof source) "The stream must support reading"
    try
        fromBytes(StreamWrapper source)
    finally
        source.Close()

let fromFile (source: FileInfo) =
    if isNull source then nullArg(nameof source)
    source.OpenRead() |> fromStream

let fromPath source = fromFile(FileInfo source)
