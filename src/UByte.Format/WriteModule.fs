module UByte.Format.WriteModule

open System
open System.IO
open System.Runtime.InteropServices

open UByte.Format.Model
open UByte.Format.Model.InstructionSet

let inline bits1 (value: 'Enum when 'Enum : enum<uint8>) (dest: Stream) = dest.WriteByte(uint8 value)

[<RequireQualifiedAccess>]
module private VarInt =
    let private integerOutOfRange value =
        raise(ArgumentOutOfRangeException(nameof value, value, "Writing of variable-length integer is out of supported range"))

    let unsigned (value: uvarint) (dest: Stream) =
        if value <= 0x7Fu then
            dest.WriteByte(uint8 value <<< 1)
        elif value <= 0x3FFFu then
            let value' = value <<< 2;
            dest.WriteByte(uint8 value' ||| 0b01uy)
            dest.WriteByte(uint8(value' >>> 8))
        elif value <= 0x1FFFFFu then
            let value' = value <<< 3;
            dest.WriteByte(uint8 value' ||| 0b011uy)
            dest.WriteByte(uint8(value' >>> 8))
            dest.WriteByte(uint8(value' >>> 16))
        else
            integerOutOfRange value

    let signed (value: varint) dest =
        if value >= int32 0xC0y && value <= 0x3F then
            unsigned (uint32(uint8 value &&& 0b0111_1111uy)) dest
        elif value >= int32 0xE000s && value <= 0x1FFF then
            unsigned (uint32(uint16 value &&& 0b0111_1111_1111_1111us)) dest
        // -524288 (0b0000_1000_0000_0000_0000_0000)
        else
            integerOutOfRange value

let inline index (Index i) dest = VarInt.unsigned i dest

let vector writer (items: vector<'T>) dest =
    VarInt.unsigned (uint32 items.Length) dest
    for i = 0 to items.Length - 1 do
        writer items.[i] dest

let versions (VersionNumbers numbers) dest = vector VarInt.unsigned numbers dest

let str (chars: string) dest =
    VarInt.unsigned (uint32 chars.Length) dest
    // NOTE: Can write strings more efficeintly by using StreamWriter or Encoder
    dest.Write(ReadOnlySpan(System.Text.Encoding.UTF8.GetBytes chars))

let name (Name name) dest = str name dest

let lengthEncodedData (buffer: MemoryStream) dest writer =
    let inline reset() = buffer.Seek(0L, SeekOrigin.Begin) |> ignore

    buffer.SetLength 0L
    reset()
    writer buffer

    VarInt.unsigned (uint32 buffer.Length) dest
    buffer.WriteTo dest

let lengthEncodedVector buffer dest items writer = lengthEncodedData buffer dest (vector writer items)

let moduleID id dest =
    name id.ModuleName dest
    versions id.Version dest

let inline opcode (code: Opcode) dest = VarInt.unsigned (uint32 code) dest

[<RequireQualifiedAccess>]
module private Constant =
    let private integer<'T when 'T : struct and 'T : (new: unit -> 'T) and 'T :> System.ValueType>
        (value: 'T)
        endianness
        (dest: Stream)
        =
        let buffer = Span.stackalloc<byte> sizeof<'T>
        let mutable value = value
        MemoryMarshal.Write(buffer, &value)

        match endianness with
        | LittleEndian when BitConverter.IsLittleEndian -> ()
        | BigEndian when not BitConverter.IsLittleEndian -> ()
        | _ -> buffer.Reverse()

        dest.Write(Span.asReadOnly buffer)

    let i32 value endianness dest = integer<int32> value endianness dest

let primitiveType prim (dest: Stream) =
    match prim with
    | PrimitiveType.S8 -> Tag.Type.S8
    | PrimitiveType.S16 -> Tag.Type.S16
    | PrimitiveType.S32 -> Tag.Type.S32
    | PrimitiveType.S64 -> Tag.Type.S64
    | PrimitiveType.U8 -> Tag.Type.U8
    | PrimitiveType.U16 -> Tag.Type.U16
    | PrimitiveType.U32 -> Tag.Type.U32
    | PrimitiveType.U64 -> Tag.Type.U64
    | PrimitiveType.UNative -> Tag.Type.UNative
    | PrimitiveType.SNative -> Tag.Type.UNative
    | PrimitiveType.F32 -> Tag.Type.F32
    | PrimitiveType.F64 -> Tag.Type.F64
    | PrimitiveType.Char16 -> Tag.Type.Char16
    | PrimitiveType.Char32 -> Tag.Type.Char32
    | PrimitiveType.Bool -> Tag.Type.Bool
    |> uint8
    |> dest.WriteByte

let instruction instr dest =
    let inline arithmeticUnaryOp i (flags: ArithmeticFlags) (rtype: TypeSignatureIndex) (register: RegisterIndex) =
        opcode i dest
        bits1 flags dest
        index rtype dest
        index register dest

    let inline arithmeticBinaryOp i flags rtype (x: RegisterIndex) (y: RegisterIndex) =
        arithmeticUnaryOp i flags rtype x
        index y dest

    let inline bitwiseUnaryOp i rtype (register: RegisterIndex) =
        opcode i dest
        primitiveType rtype dest
        index register dest

    let inline bitwiseBinaryOp i rtype x (y: RegisterIndex) =
        bitwiseUnaryOp i rtype x
        index y dest

    let inline boffset (offset: BlockOffset) = VarInt.signed offset dest

    let inline branchComparisonInstruction i x y btrue bfalse =
        opcode i dest
        index x dest
        index y dest
        boffset btrue
        boffset bfalse

    match instr with
    | Nop -> opcode Opcode.nop dest
    | Ret registers ->
        opcode Opcode.ret dest
        vector index registers dest
    | Phi values ->
        opcode Opcode.phi dest
        vector (fun struct(rindex, bindex) dest -> index rindex dest; boffset bindex) values dest
    | Select(condition, vtrue, vfalse) ->
        opcode Opcode.select dest
        index condition dest
        index vtrue dest
        index vfalse dest
    | Call(flags, method, arguments) ->
        opcode Opcode.call dest
        bits1 flags dest
        index method dest
        vector index arguments dest
    | Call_virt(flags, method, this, arguments) ->
        opcode Opcode.``call.virt`` dest
        bits1 flags dest
        index method dest
        index this dest
        vector index arguments dest
    | Add(flags, rtype, x, y) -> arithmeticBinaryOp Opcode.add flags rtype x y
    | Sub(flags, rtype, x, y) -> arithmeticBinaryOp Opcode.sub flags rtype x y
    | Mul(flags, rtype, x, y) -> arithmeticBinaryOp Opcode.mul flags rtype x y
    | Div(flags, rtype, x, y) -> arithmeticBinaryOp Opcode.div flags rtype x y
    | Incr(flags, rtype, reg) -> arithmeticUnaryOp Opcode.incr flags rtype reg
    | Decr(flags, rtype, reg) -> arithmeticUnaryOp Opcode.decr flags rtype reg
    | And(rtype, x, y) -> bitwiseBinaryOp Opcode.``and`` rtype x y
    | Or(rtype, x, y) -> bitwiseBinaryOp Opcode.``or`` rtype x y
    | Not(rtype, reg) -> bitwiseUnaryOp Opcode.``not`` rtype reg
    | Xor(rtype, x, y) -> bitwiseBinaryOp Opcode.``xor`` rtype x y
    | Rem(flags, rtype, x, y) -> arithmeticBinaryOp Opcode.rem flags rtype x y
    | Rotl(rtype, amt, reg) -> bitwiseBinaryOp Opcode.rotl rtype amt reg
    | Rotr(rtype, amt, reg) -> bitwiseBinaryOp Opcode.rotr rtype amt reg
    | Const_i(vtype, value) ->
        opcode Opcode.``const.i`` dest
        primitiveType vtype dest
        VarInt.signed value dest
    | Const_f32 _
    | Const_f64 _ -> failwith "TODO: How to handle endianness of floating point values"
    | Const_true vtype ->
        opcode Opcode.``const.true`` dest
        primitiveType vtype dest
    | Const_false vtype
    | Const_zero vtype ->
        opcode Opcode.``const.zero`` dest
        primitiveType vtype dest
    | Br target ->
        opcode Opcode.br dest
        boffset target
    | Br_eq(x, y, btrue, bfalse) -> branchComparisonInstruction Opcode.``br.eq`` x y btrue bfalse
    | Br_ne(x, y, btrue, bfalse) -> branchComparisonInstruction Opcode.``br.ne`` x y btrue bfalse
    | Br_lt(x, y, btrue, bfalse) -> branchComparisonInstruction Opcode.``br.lt`` x y btrue bfalse
    | Br_gt(x, y, btrue, bfalse) -> branchComparisonInstruction Opcode.``br.gt`` x y btrue bfalse
    | Br_le(x, y, btrue, bfalse) -> branchComparisonInstruction Opcode.``br.le`` x y btrue bfalse
    | Br_ge(x, y, btrue, bfalse) -> branchComparisonInstruction Opcode.``br.ge`` x y btrue bfalse
    | Br_true(cond, btrue, bfalse) ->
        opcode Opcode.br dest
        index cond dest
        boffset btrue
        boffset bfalse
    | Mem_init(flags, count, ty, addr, value) ->
        opcode Opcode.``mem.init`` dest
        bits1 flags dest
        index count dest
        index ty dest
        index addr dest
        index value dest
    | Mem_st(flags, value, ty, addr) ->
        opcode Opcode.``mem.st`` dest
        bits1 flags dest
        index value dest
        index ty dest
        index addr dest
    | Mem_cpy(flags, count, ty, sreg, dreg) ->
        opcode Opcode.``mem.cpy`` dest
        bits1 flags dest
        index count dest
        index ty dest
        index sreg dest
        index dreg dest
    | Mem_ld(flags, ty, addr) ->
        opcode Opcode.``mem.ld`` dest
        bits1 flags dest
        index ty dest
        index addr dest
    | Mem_init_const(flags, ty, addr, data) ->
        opcode Opcode.``mem.init.const`` dest
        bits1 flags dest
        index ty dest
        index addr dest
        index data dest
    | Obj_new(ctor, args) ->
        opcode Opcode.``obj.new`` dest
        index ctor dest
        vector index args dest
    | Obj_null -> opcode Opcode.``obj.null`` dest
    | Obj_fd_ld(field, object) ->
        opcode Opcode.``obj.fd.ld`` dest
        index field dest
        index object dest
    | Obj_fd_st(field, object, src) ->
        opcode Opcode.``obj.fd.st`` dest
        index field dest
        index object dest
        index src dest
    | Obj_fd_addr(flags, field, object) ->
        opcode Opcode.``obj.fd.addr`` dest
        bits1 flags dest
        index field dest
        index object dest
    | Obj_throw ex ->
        opcode Opcode.``obj.throw`` dest
        index ex dest
    | Obj_arr_new(etype, len) ->
        opcode Opcode.``obj.arr.new`` dest
        index etype dest
        index len dest
    | Obj_arr_len(flags, ltype, array) ->
        opcode Opcode.``obj.arr.len`` dest
        bits1 flags dest
        primitiveType ltype dest
        index array dest
    | Obj_arr_get(array, i) ->
        opcode Opcode.``obj.arr.get`` dest
        index array dest
        index i dest
    | Obj_arr_set(array, i, src) ->
        opcode Opcode.``obj.arr.set`` dest
        index array dest
        index i dest
        index src dest
    | Obj_arr_addr(flags, array, i) ->
        opcode Opcode.``obj.arr.addr`` dest
        bits1 flags dest
        index array dest
        index i dest
    | Obj_arr_const(etype, data) ->
        opcode Opcode.``obj.arr.const`` dest
        index etype dest
        index data dest
    | Alloca(count, ty) ->
        opcode Opcode.alloca dest
        index count dest
        index ty dest

let localRegisterMapping (struct(tindex: TemporaryIndex, lindex: LocalIndex)) dest =
    index tindex dest
    index lindex dest

let block auxbuf (block: CodeBlock) (dest: Stream) =
    bits1 block.Flags dest

    match block.ExceptionHandler with
    | ValueSome({ ExceptionRegister = ValueSome eindex } as eh) ->
        index eindex dest
        index eh.CatchBlock dest
    | ValueSome { ExceptionRegister = ValueNone; CatchBlock = cindex } ->
        index cindex dest
    | ValueNone -> ()

    vector localRegisterMapping block.Locals dest
    lengthEncodedVector auxbuf dest block.Instructions instruction

let toStream (stream: Stream) (md: Module) =
    if isNull stream then nullArg(nameof stream)
    if not stream.CanWrite then invalidArg (nameof stream) "The stream must support writing"
    try
        let buffer = new MemoryStream()
        let (Magic magic) = md.Magic
        stream.Write(magic.AsSpan())
        versions md.FormatVersion stream
        VarInt.unsigned md.DataVectorCount stream

        lengthEncodedData buffer stream <| fun dest ->
            let header = md.Header
            VarInt.unsigned header.FieldCount dest
            moduleID header.Module dest
            bits1 header.Flags dest
            bits1 header.PointerSize dest

        lengthEncodedVector buffer stream md.Identifiers.Identifiers str

        lengthEncodedVector buffer stream md.Namespaces (vector index)

        lengthEncodedVector buffer stream md.TypeSignatures <| fun signature dest ->
            let inline ttag (t: Tag.Type) = dest.WriteByte(uint8 t)

            let rec vtype =
                function
                | ValueType.Primitive prim -> primitiveType prim dest
                | ValueType.Defined tindex ->
                    ttag Tag.Type.DefinedStruct
                    index tindex dest
                | ValueType.UnsafePointer ttype ->
                    ttag Tag.Type.UnsafePointer
                    vtype ttype

            let rec rtype =
                function
                | ReferenceType.Defined tindex ->
                    ttag Tag.Type.RefDefinedType
                    index tindex dest
                | ReferenceType.BoxedValueType btype ->
                    ttag Tag.Type.RefBoxed
                    vtype btype
                | ReferenceType.Vector etype ->
                    ttag Tag.Type.RefVector
                    rvtype etype
                | ReferenceType.Any ->
                    ttag Tag.Type.RefAny

            and rvtype =
                function
                | ReferenceOrValueType.Reference rt -> rtype rt
                | ReferenceOrValueType.Value vt -> vtype vt

            match signature with
            | ValueType vt -> vtype vt
            | ReferenceType rt -> rtype rt
            | SafePointer t ->
                ttag Tag.Type.SafePointer
                rvtype t

        lengthEncodedVector buffer stream md.MethodSignatures <| fun signature dest ->
            vector index signature.ReturnTypes dest
            vector index signature.ParameterTypes dest

        let auxbuf = new MemoryStream()

        lengthEncodedData buffer stream <| fun dest ->
            vector moduleID md.Imports.ImportedModules dest

            lengthEncodedVector auxbuf dest md.Imports.ImportedTypes <| fun timport dest ->
                index timport.Module dest
                index timport.TypeName dest
                index timport.TypeNamespace dest
                VarInt.unsigned timport.TypeParameters dest
                if timport.TypeParameters > 0u then failwith "TODO: Type imports with type parameters are not yet supported"

            lengthEncodedVector auxbuf dest md.Imports.ImportedFields <| fun fimport dest ->
                index fimport.FieldOwner dest
                index fimport.FieldName dest
                index fimport.FieldType dest

            lengthEncodedVector auxbuf dest md.Imports.ImportedMethods <| fun mimport dest ->
                index mimport.MethodOwner dest
                index mimport.MethodName dest
                VarInt.unsigned mimport.TypeParameters dest
                if mimport.TypeParameters > 0u then failwith "TODO: Method imports with type parameters are not yet supported"
                index mimport.Signature dest

        lengthEncodedData buffer stream <| fun dest ->
            lengthEncodedVector auxbuf dest md.Definitions.DefinedTypes <| fun t dest ->
                index t.TypeName dest
                index t.TypeNamespace dest
                bits1 t.TypeVisibility dest
                bits1 t.TypeFlags dest

                match t.TypeLayout with
                | TypeDefinitionLayout.Unspecified ->
                    bits1 Tag.TypeDefinitionLayout.Unspecified dest
                | TypeDefinitionLayout.Sequential ->
                    bits1 Tag.TypeDefinitionLayout.Sequential dest

                if not t.TypeParameters.IsDefaultOrEmpty then failwith "TODO: Generic types not yet supported"
                dest.WriteByte 0uy // length of vector
                vector index t.InheritedTypes dest
                if not t.TypeAnnotations.IsDefaultOrEmpty then failwith "TODO: Annotated types not yet supported"
                dest.WriteByte 0uy // length of vector
                vector index t.Fields dest
                vector index t.Methods dest
                vector (fun ov dest -> index ov.Declaration dest; index ov.Implementation dest) t.VTable dest

            lengthEncodedVector auxbuf dest md.Definitions.DefinedFields <| fun f dest ->
                index f.FieldOwner dest
                index f.FieldName dest
                bits1 f.FieldVisibility dest
                bits1 f.FieldFlags dest
                index f.FieldType dest
                if not f.FieldAnnotations.IsDefaultOrEmpty then failwith "TODO: Annotated fields not yet supported"
                dest.WriteByte 0uy // length of vector

            lengthEncodedVector auxbuf dest md.Definitions.DefinedMethods <| fun m dest ->
                index m.MethodOwner dest
                index m.MethodName dest
                bits1 m.MethodVisibility dest
                bits1 m.MethodFlags dest
                if not m.TypeParameters.IsDefaultOrEmpty then failwith "TODO: Generic methods not yet supported"
                dest.WriteByte 0uy // length of vector
                index m.Signature dest
                if not m.MethodAnnotations.IsDefaultOrEmpty then failwith "TODO: Annotated methods not yet supported"
                dest.WriteByte 0uy // length of vector

                match m.Body with
                | MethodBody.Defined codei ->
                    bits1 Tag.MethodBody.Defined dest
                    index codei dest
                | MethodBody.Abstract -> bits1 Tag.MethodBody.Abstract dest
                | MethodBody.External(library, func) ->
                    bits1 Tag.MethodBody.External dest
                    index library dest
                    index func dest

        lengthEncodedVector buffer stream md.Data <| fun data dest ->
            lengthEncodedData auxbuf dest <| fun dest' -> dest'.Write(data.AsSpan())

        lengthEncodedVector buffer stream md.Code <| fun code dest ->
            VarInt.unsigned code.LocalCount dest
            vector (block auxbuf) code.Blocks dest

        match md.EntryPoint with
        | ValueNone -> VarInt.unsigned 0u stream
        | ValueSome main -> lengthEncodedData buffer stream (index main)

        // Debug info not yet supported.
        stream.WriteByte 0uy // length of data
    finally
        stream.Close()

let toFile (destination: FileInfo) md =
    if isNull destination then nullArg(nameof destination)
    toStream (destination.OpenWrite()) md

let toPath destination md = toFile (FileInfo destination) md
