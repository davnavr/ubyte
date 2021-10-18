module UByte.Format.WriteModule

open System
open System.IO
open System.Runtime.InteropServices

open UByte.Format.Model
open UByte.Format.Model.InstructionSet

let inline bits1 (value: 'Enum when 'Enum : enum<uint8>) (dest: Stream) = dest.WriteByte(uint8 value)

[<RequireQualifiedAccess>]
module VarInt =
    let integerOutOfRange value =
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

let registerType (struct(count: uvarint, t: RegisterType)) dest =
    VarInt.unsigned count dest
    index t.RegisterType dest
    bits1 t.RegisterFlags dest

let inline opcode (code: Opcode) dest = VarInt.unsigned (uint32 code) dest

[<RequireQualifiedAccess>]
module Constant =
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

let instruction endianness i dest =
    let inline (|Opcode|) op _: Opcode = op

    match i with
    | Instruction.Nop -> opcode Opcode.nop dest
    | Instruction.Ret registers ->
        opcode Opcode.ret dest
        vector index registers dest
    | (InstructionSet.Incr reg & Opcode Opcode.incr op)
    | (InstructionSet.Decr reg & Opcode Opcode.decr op) ->
        opcode op dest
        index reg dest
    | (Instruction.Call(method, aregs, rregs) & Opcode Opcode.call op)
    | (Instruction.Call_virt(method, aregs, rregs) & Opcode Opcode.``call.virt`` op)
    | (Instruction.Call_ret(method, aregs, rregs) & Opcode Opcode.``call.ret`` op)
    | (Instruction.Call_virt_ret(method, aregs, rregs) & Opcode Opcode.``call.virt.ret`` op) ->
        opcode op dest
        index method dest
        vector index aregs dest
        vector index rregs dest
    | (Instruction.Reg_copy(reg1, reg2) & Opcode Opcode.``reg.copy`` op)
    | (Instruction.Rotl(reg1, reg2) & Opcode Opcode.rotl op)
    | (Instruction.Rotr(reg1, reg2) & Opcode Opcode.rotr op)
    | (Instruction.Obj_arr_len(reg1, reg2) & Opcode Opcode.``obj.arr.len`` op) ->
        opcode op dest
        index reg1 dest
        index reg2 dest
    | (Instruction.Add(xreg, yreg, rreg) & Opcode Opcode.add op)
    | (Instruction.Sub(xreg, yreg, rreg) & Opcode Opcode.sub op)
    | (Instruction.Mul(xreg, yreg, rreg) & Opcode Opcode.mul op)
    | (Instruction.Add_ovf(xreg, yreg, rreg) & Opcode Opcode.``add.ovf`` op)
    | (Instruction.Sub_ovf(xreg, yreg, rreg) & Opcode Opcode.``sub.ovf`` op)
    | (Instruction.Mul_ovf(xreg, yreg, rreg) & Opcode Opcode.``mul.ovf`` op)
    | (Instruction.Div(xreg, yreg, rreg) & Opcode Opcode.div op)
    | (Instruction.And(xreg, yreg, rreg) & Opcode Opcode.``and`` op)
    | (Instruction.Or(xreg, yreg, rreg) & Opcode Opcode.``or`` op)
    | (Instruction.Not(xreg, yreg, rreg) & Opcode Opcode.``not`` op)
    | (Instruction.Xor(xreg, yreg, rreg) & Opcode Opcode.xor op)
    | (Instruction.Rem(xreg, yreg, rreg) & Opcode Opcode.rem op)
    | (Instruction.Obj_arr_get(xreg, yreg, rreg) & Opcode Opcode.``obj.arr.get`` op)
    | (Instruction.Obj_arr_set(xreg, yreg, rreg) & Opcode Opcode.``obj.arr.set`` op) ->
        opcode op dest
        index xreg dest
        index yreg dest
        index rreg dest
    | Instruction.Const_i32(value, rreg) ->
        opcode Opcode.``const.i32`` dest
        Constant.i32 value endianness dest
        index rreg dest
    | (Instruction.Const_true reg & Opcode Opcode.``const.true`` op)
    | (Instruction.Const_false reg & Opcode Opcode.``const.false`` op)
    | (Instruction.Const_zero reg & Opcode Opcode.``const.zero`` op)
    | (Instruction.Obj_null reg & Opcode Opcode.``obj.null`` op) ->
        opcode op dest
        index reg dest
    | Instruction.Br target ->
        opcode Opcode.br dest
        VarInt.signed target dest
    | (Instruction.Br_eq(xreg, yreg, target) & Opcode Opcode.``br.eq`` op)
    | (Instruction.Br_ne(xreg, yreg, target) & Opcode Opcode.``br.ne`` op)
    | (Instruction.Br_lt(xreg, yreg, target) & Opcode Opcode.``br.lt`` op)
    | (Instruction.Br_gt(xreg, yreg, target) & Opcode Opcode.``br.gt`` op)
    | (Instruction.Br_le(xreg, yreg, target) & Opcode Opcode.``br.le`` op)
    | (Instruction.Br_ge(xreg, yreg, target) & Opcode Opcode.``br.ge`` op) ->
        opcode op dest
        index xreg dest
        index yreg dest
        VarInt.signed target dest
    | (Instruction.Br_true(reg, target) & Opcode Opcode.``br.true`` op)
    | (Instruction.Br_false(reg, target) & Opcode Opcode.``br.false`` op) ->
        opcode op dest
        index reg dest
        VarInt.signed target dest
    | Instruction.Obj_new(constructor, aregs, rreg) ->
        opcode Opcode.``obj.new`` dest
        index constructor dest
        vector index aregs dest
        index rreg dest
    | (Instruction.Obj_ldfd(field, typei, reg) & Opcode Opcode.``obj.ldfd`` op)
    | (Instruction.Obj_stfd(field, typei, reg) & Opcode Opcode.``obj.stfd`` op) ->
        opcode op dest
        index field dest
        index typei dest
        index reg dest
    | Instruction.Obj_arr_new(typei, lreg, rreg) ->
        opcode Opcode.``obj.arr.new`` dest
        index typei dest
        index lreg dest
        index rreg dest
    | _ -> failwithf "TODO: Cannot write unsupported instruction %A" i

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
                | ValueType.Primitive prim ->
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
                    | PrimitiveType.Unit -> Tag.Type.Unit
                    |> ttag
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
                dest.WriteByte(if timport.IsStruct then 1uy else 0uy)
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

                match t.TypeKind with
                | Class(extends, flags) ->
                    match extends with
                    | ValueSome basei ->
                        bits1 Tag.TypeDefinitionKind.Class dest
                        index basei dest
                    | ValueNone -> bits1 Tag.TypeDefinitionKind.BaseClass dest

                    bits1 flags dest
                | Interface ->
                    bits1 Tag.TypeDefinitionKind.Interface dest
                | Struct ->
                    bits1 Tag.TypeDefinitionKind.Struct dest

                match t.TypeLayout with
                | TypeDefinitionLayout.Unspecified ->
                    bits1 Tag.TypeDefinitionLayout.Unspecified dest
                | TypeDefinitionLayout.Sequential ->
                    bits1 Tag.TypeDefinitionLayout.Sequential dest

                if not t.ImplementedInterfaces.IsDefaultOrEmpty then failwith "TODO: Writing of implemented interfaces not yet supported"
                dest.WriteByte 0uy // length of vector
                if not t.TypeParameters.IsDefaultOrEmpty then failwith "TODO: Generic types not yet supported"
                dest.WriteByte 0uy // length of vector
                if not t.TypeAnnotations.IsDefaultOrEmpty then failwith "TODO: Annotated types not yet supported"
                dest.WriteByte 0uy // length of vector

                vector index t.Fields dest
                vector index t.Methods dest

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

        lengthEncodedVector buffer stream md.Data (fun data dest -> dest.Write(data.AsSpan()))

        lengthEncodedVector buffer stream md.Code <| fun code dest ->
            vector registerType code.RegisterTypes dest
            lengthEncodedVector auxbuf dest code.Instructions (instruction md.Endianness)

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
