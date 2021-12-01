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
module private VarInt =
    let private integerOutOfRange() = raise(NotSupportedException "Cannot parse excessively large integer")

    let unsigned (stream: #IByteSequence): uvarint =
        let first = u1 stream
        if first &&& 1uy = 0uy then
            uint32(first >>> 1)
        elif first &&& 0b11uy = 1uy then
            let second = u1 stream
            (uint32 first >>> 2) ||| (uint32 second <<< 6)
        elif first &&& 0b111uy = 0b11uy then
            let second = u1 stream
            let third = u1 stream
            (uint32 first >>> 3) ||| (uint32 second <<< 5) ||| (uint32 third <<< 13)
        else
            integerOutOfRange()

    let signed stream: varint =
        let value = unsigned stream
        if value <= 0x7Fu then
            let mutable value' = uint8 value
            if 0b0100_0000uy &&& value' <> 0uy then
                value' <- value' ||| 0b1000_0000uy
            int32(int8 value')
        elif value <= 0x3FFFu then
            let mutable value' = uint16 value
            if 0b0010_0000_0000_0000us &&& value' <> 0us then
                value' <- value' ||| 0b1100_0000_0000_0000us
            int32(int16 value')
        else
            integerOutOfRange()

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
        let length = Checked.int32(VarInt.unsigned source)
        Array.zeroCreate<'Result> length

    for i = 0 to items.Length - 1 do
        items.[i] <- parse source

    Unsafe.As<_, ImmutableArray<'Result>> &items

let inline versions (source: #_) = VersionNumbers(vector source VarInt.unsigned)

let inline index (source: #_) = Index(VarInt.unsigned source)

let lengthEncodedData (source: #IByteSequence) =
    let mutable data =
        VarInt.unsigned source
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
    let fcount = VarInt.unsigned header'
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
              TypeNamespace = index t
              TypeParameters = VarInt.unsigned t }
      ImportedFields =
        lengthEncodedVector source <| fun f ->
            { FieldImport.FieldOwner = index f
              FieldName = index f
              FieldType = index f }
      ImportedMethods =
        lengthEncodedVector source <| fun m ->
            { MethodImport.MethodOwner = index m
              MethodName = index m
              TypeParameters = VarInt.unsigned m
              Signature = index m } }

let annotation _ = failwith "TODO: Annotations not yet supported"

let genericTypeParam _ = failwith "TODO: Type parameters not yet supported"

let moduleDefinitions source =
    { ModuleDefinitions.DefinedTypes =
        lengthEncodedVector source <| fun t ->
            { TypeDefinition.TypeName = index t
              TypeNamespace = index t
              TypeVisibility = bits1 t
              TypeFlags = bits1 t
              TypeLayout =
                match bits1 t with
                | Tag.TypeDefinitionLayout.Unspecified -> TypeDefinitionLayout.Unspecified
                | Tag.TypeDefinitionLayout.Sequential -> TypeDefinitionLayout.Sequential
                | bad -> failwithf "TODO: Bad type definition layout kind 0x%02X" (uint8 bad)
              TypeParameters = vector t genericTypeParam
              InheritedTypes = vector t index
              TypeAnnotations = vector t annotation
              Fields = vector t index
              Methods = vector t index
              VTable = vector t <| fun ov -> { MethodOverride.Declaration = index ov; Implementation = index ov } }
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
                | Tag.MethodBody.External -> MethodBody.External(index m, index m)
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

let rec parsedPrimitiveType tag success continuation =
    match tag with
    | Tag.Type.S8 -> success PrimitiveType.S8
    | Tag.Type.S16 -> success PrimitiveType.S16
    | Tag.Type.S32 -> success PrimitiveType.S32
    | Tag.Type.S64 -> success PrimitiveType.S64
    | Tag.Type.U8 -> success PrimitiveType.U8
    | Tag.Type.U16 -> success PrimitiveType.U16
    | Tag.Type.U32 -> success PrimitiveType.U32
    | Tag.Type.U64 -> success PrimitiveType.U64
    | Tag.Type.F32 -> success PrimitiveType.F32
    | Tag.Type.F64 -> success PrimitiveType.F64
    | Tag.Type.Bool -> success PrimitiveType.Bool
    | Tag.Type.Char16 -> success PrimitiveType.Char16
    | Tag.Type.Char32 -> success PrimitiveType.Char32
    | Tag.Type.UNative -> success PrimitiveType.UNative
    | Tag.Type.SNative -> success PrimitiveType.SNative
    | bad -> continuation bad

let instructionPrimitiveType source = parsedPrimitiveType (bits1 source) id (failwithf "%A is not a valid primitive type")

let instruction source =
    let inline arithmeticUnaryOp instr: Instruction =
        instr(bits1 source, index source, index source)

    let inline arithmeticBinaryOp instr: Instruction =
        instr(bits1 source, index source, index source, index source)

    let inline bitwiseUnaryOp instr: Instruction =
        instr(instructionPrimitiveType source, index source)

    let inline bitwiseBinaryOp instr: Instruction =
        instr(instructionPrimitiveType source, index source, index source)

    let inline boffset(): BlockOffset = VarInt.signed source

    let inline branchComparisonInstruction instr: Instruction =
        instr(index source, index source, boffset(), boffset())

    let inline instr2 instr: Instruction = instr(index source, index source)
    let inline instr3 instr: Instruction = instr(index source, index source, index source)

    match LanguagePrimitives.EnumOfValue(VarInt.unsigned source) with
    | Opcode.nop -> Nop
    | Opcode.ret -> Ret(vector source index)
    | Opcode.phi -> Phi(vector source (fun pair -> struct(index pair, VarInt.signed pair)))
    | Opcode.select -> instr3 Select
    | Opcode.call -> Call(bits1 source, index source, vector source index)
    | Opcode.``call.virt`` -> Call_virt(bits1 source, index source, index source, vector source index)
    | Opcode.add -> arithmeticBinaryOp Add
    | Opcode.sub -> arithmeticBinaryOp Sub
    | Opcode.mul -> arithmeticBinaryOp Mul
    | Opcode.div -> arithmeticBinaryOp Div
    | Opcode.incr -> arithmeticUnaryOp Incr
    | Opcode.decr -> arithmeticUnaryOp Decr
    | Opcode.``and`` -> bitwiseBinaryOp And
    | Opcode.``or`` -> bitwiseBinaryOp Or
    | Opcode.``not`` -> bitwiseUnaryOp Not
    | Opcode.``xor`` -> bitwiseBinaryOp Xor
    | Opcode.rem -> arithmeticBinaryOp Rem
    | Opcode.rotl -> bitwiseBinaryOp Rotl
    | Opcode.rotr -> bitwiseBinaryOp Rotr
    | Opcode.``const.i`` -> Const_i(instructionPrimitiveType source, VarInt.signed source)
    //| Opcode.``const.f32`` ->
    //| Opcode.``const.f64`` ->
    | Opcode.``const.true`` -> Const_true(instructionPrimitiveType source)
    | Opcode.``const.zero`` (*| Opcode.``const.false``*) -> Const_zero(instructionPrimitiveType source)
    | Opcode.br -> Br(boffset())
    | Opcode.``br.eq`` -> branchComparisonInstruction Br_eq
    | Opcode.``br.ne`` -> branchComparisonInstruction Br_ne
    | Opcode.``br.lt`` -> branchComparisonInstruction Br_lt
    | Opcode.``br.gt`` -> branchComparisonInstruction Br_gt
    | Opcode.``br.le`` -> branchComparisonInstruction Br_le
    | Opcode.``br.ge`` -> branchComparisonInstruction Br_ge
    | Opcode.``br.true`` -> Br_true(index source, boffset(), boffset())
    | Opcode.``cmp.eq`` -> instr2 Cmp_eq
    | Opcode.``cmp.ne`` -> instr2 Cmp_ne
    | Opcode.``cmp.lt`` -> instr2 Cmp_lt
    | Opcode.``cmp.gt`` -> instr2 Cmp_gt
    | Opcode.``cmp.le`` -> instr2 Cmp_le
    | Opcode.``cmp.ge`` -> instr2 Cmp_ge
    | Opcode.``mem.init`` -> Mem_init(bits1 source, index source, index source, index source, index source)
    | Opcode.``mem.st`` -> Mem_st(bits1 source, index source, index source, index source)
    | Opcode.``mem.cpy`` -> Mem_cpy(bits1 source, index source, index source, index source, index source)
    | Opcode.``mem.ld`` -> Mem_ld(bits1 source, index source, index source)
    | Opcode.``mem.init.const`` -> Mem_init_const(bits1 source, index source, index source, index source)
    | Opcode.``obj.new`` -> Obj_new(index source, vector source index)
    | Opcode.``obj.null`` -> Obj_null
    | Opcode.``obj.fd.ld`` -> instr2 Obj_fd_ld
    | Opcode.``obj.fd.st`` -> instr3 Obj_fd_st
    | Opcode.``obj.fd.addr`` -> Obj_fd_addr(bits1 source, index source, index source)
    | Opcode.``obj.throw`` -> Obj_throw(index source)
    | Opcode.``obj.arr.new`` -> instr2 Obj_arr_new
    | Opcode.``obj.arr.len`` -> Obj_arr_len(bits1 source, instructionPrimitiveType source, index source)
    | Opcode.``obj.arr.get`` -> instr2 Obj_arr_get
    | Opcode.``obj.arr.set`` -> instr3 Obj_arr_set
    | Opcode.``obj.arr.addr`` -> Obj_arr_addr(bits1 source, index source, index source)
    | Opcode.``obj.arr.const`` -> instr2 Obj_arr_const
    | Opcode.alloca -> Alloca(index source, index source)
    | bad -> failwithf "TODO: Unrecognized opcode 0x%08X (%A)" (uint32 bad) bad

let block source =
    let flags = bits1 source
    { CodeBlock.ExceptionHandler =
        match flags &&& CodeBlockFlags.ExceptionHandlingMask with
        | CodeBlockFlags.None -> ValueNone
        | CodeBlockFlags.ExceptionHandlerStoresException ->
            ValueSome { BlockExceptionHandler.ExceptionRegister = ValueSome(index source); CatchBlock = index source }
        | CodeBlockFlags.ExceptionHandlerIgnoresException ->
            ValueSome { BlockExceptionHandler.ExceptionRegister = ValueNone; CatchBlock = index source}
        | bad -> failwithf "TODO: Error for invalid code block exception handling kind (0x%02X) %A" (uint8 bad) bad
      Locals = vector source (fun source -> struct(index source, index source))
      Instructions = lengthEncodedVector source instruction }

let fromBytes (source: #IByteSequence) =
    let magic' = magic source
    let fversion = versions source
    if fversion <> Model.currentFormatVersion then failwithf "TODO: Error for unsupported version %O" fversion
    let dcount = VarInt.unsigned source
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
            let inline valtype t = ValueType t
            let inline reftype t = ReferenceType t
            let inline primitive t = valtype (ValueType.Primitive t)

            let rec parsedValueType tag success continuation =
                let inline primitive t = success (ValueType.Primitive t)
            
                match tag with // TODO: Avoid code duplication with parsedPrimitiveType
                | Tag.Type.S8 -> primitive PrimitiveType.S8
                | Tag.Type.S16 -> primitive PrimitiveType.S16
                | Tag.Type.S32 -> primitive PrimitiveType.S32
                | Tag.Type.S64 -> primitive PrimitiveType.S64
                | Tag.Type.U8 -> primitive PrimitiveType.U8
                | Tag.Type.U16 -> primitive PrimitiveType.U16
                | Tag.Type.U32 -> primitive PrimitiveType.U32
                | Tag.Type.U64 -> primitive PrimitiveType.U64
                | Tag.Type.F32 -> primitive PrimitiveType.F32
                | Tag.Type.F64 -> primitive PrimitiveType.F64
                | Tag.Type.Bool -> primitive PrimitiveType.Bool
                | Tag.Type.Char16 -> primitive PrimitiveType.Char16
                | Tag.Type.Char32 -> primitive PrimitiveType.Char32
                | Tag.Type.UNative -> primitive PrimitiveType.UNative
                | Tag.Type.SNative -> primitive PrimitiveType.SNative
                | Tag.Type.UnsafePointer -> parsedValueType (bits1 tsig) (ValueType.UnsafePointer >> success) continuation
                | bad -> continuation bad

            // Explicit instantiation gets around "less generic" errors with mutually recursive functions.
            let parsedReferenceOrValueType' = ref Unchecked.defaultof<_ -> (_ -> ReferenceOrValueType) -> ReferenceOrValueType>

            let parsedReferenceType tag (success: _ -> 'T) continuation = // TODO: Bad less generic errors because of recursive stuff
                match tag with
                | Tag.Type.RefAny -> success ReferenceType.Any
                | Tag.Type.RefDefinedType -> success (ReferenceType.Defined(index tsig))
                | Tag.Type.RefVector -> success (ReferenceType.Vector(parsedReferenceOrValueType'.contents (bits1 tsig) id))
                | bad -> continuation bad

            let parsedReferenceOrValueType tag (success: ReferenceOrValueType -> 'T): 'T =
                parsedReferenceType tag (ReferenceOrValueType.Reference >> success) <| fun tag ->
                    parsedValueType tag (ReferenceOrValueType.Value >> success) <| fun tag ->
                        failwithf "TODO: Error for type is not a valid reference or value type %A" tag

            parsedReferenceOrValueType' := parsedReferenceOrValueType

            match bits1 tsig with
            | Tag.Type.SafePointer -> parsedReferenceOrValueType (bits1 tsig) SafePointer
            | tag ->
                parsedReferenceOrValueType tag <| fun t ->
                    match t with
                    | ReferenceOrValueType.Reference rt -> ReferenceType rt
                    | ReferenceOrValueType.Value vt -> ValueType vt
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
        lengthEncodedVector source <| fun code ->
            { Code.LocalCount = VarInt.unsigned code
              Blocks = vector code block }
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
