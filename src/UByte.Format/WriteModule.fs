﻿module UByte.Format.WriteModule

open System
open System.IO
open System.Runtime.InteropServices

open UByte.Format.Model
open UByte.Format.Model.InstructionSet

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

let inline index (Index i) dest = LEB128.uint i dest

let vector writer (items: vector<'T>) dest =
    LEB128.uint (uint32 items.Length) dest
    for i = 0 to items.Length - 1 do
        writer items.[i] dest

let versions (VersionNumbers numbers) dest = vector LEB128.uint numbers dest

let str (chars: string) dest =
    LEB128.uint (uint32 chars.Length) dest
    // NOTE: Can write strings more efficeintly by using StreamWriter or Encoder
    dest.Write(ReadOnlySpan(System.Text.Encoding.UTF8.GetBytes chars))

let name (Name name) dest = str name dest

let lengthEncodedData (buffer: MemoryStream) dest writer =
    let inline reset() = buffer.Seek(0L, SeekOrigin.Begin) |> ignore

    buffer.SetLength 0L
    reset()
    writer buffer

    LEB128.uint (uint32 buffer.Length) dest
    buffer.WriteTo dest

let lengthEncodedVector buffer dest items writer = lengthEncodedData buffer dest (vector writer items)

let moduleID id dest =
    name id.ModuleName dest
    versions id.Version dest

let registerType (struct(count: uvarint, t: RegisterType)) dest =
    LEB128.uint count dest
    index t.RegisterType dest
    bits1 t.RegisterFlags dest

let inline opcode (code: Opcode) dest = LEB128.uint (uint32 code) dest

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
    | (Instruction.Rotr(reg1, reg2) & Opcode Opcode.rotr op) ->
        opcode op dest
        index reg1 dest
        index reg2 dest
    | (Instruction.Add(xreg, yreg, rreg) & Opcode Opcode.add op)
    | (Instruction.Sub(xreg, yreg, rreg) & Opcode Opcode.sub op)
    | (Instruction.Mul(xreg, yreg, rreg) & Opcode Opcode.mul op)
    | (Instruction.Div(xreg, yreg, rreg) & Opcode Opcode.div op)
    | (Instruction.And(xreg, yreg, rreg) & Opcode Opcode.``and`` op)
    | (Instruction.Or(xreg, yreg, rreg) & Opcode Opcode.``or`` op)
    | (Instruction.Not(xreg, yreg, rreg) & Opcode Opcode.``not`` op)
    | (Instruction.Xor(xreg, yreg, rreg) & Opcode Opcode.xor op)
    | (Instruction.Rem(xreg, yreg, rreg) & Opcode Opcode.rem op) ->
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
    | _ -> failwithf "TODO: Cannot write unsupported instruction %A" i

let toStream (stream: Stream) (md: Module) =
    if isNull stream then nullArg(nameof stream)
    if not stream.CanWrite then invalidArg (nameof stream) "The stream must support writing"
    try
        let buffer = new MemoryStream()
        let (Magic magic) = md.Magic
        stream.Write(magic.AsSpan())
        versions md.FormatVersion stream
        LEB128.uint md.DataVectorCount stream

        lengthEncodedData buffer stream <| fun dest ->
            let header = md.Header
            LEB128.uint header.FieldCount dest
            moduleID header.Module dest
            bits1 header.Flags dest
            bits1 header.PointerSize dest

        lengthEncodedVector buffer stream md.Identifiers.Identifiers str

        lengthEncodedVector buffer stream md.Namespaces (vector index)

        lengthEncodedVector buffer stream md.TypeSignatures <| fun signature dest ->
            match signature with
            | Primitive prim ->
                let tag =
                    match prim with
                    | PrimitiveType.S8 -> Tag.Type.S8
                    | PrimitiveType.S16 -> Tag.Type.S16
                    | PrimitiveType.S32 -> Tag.Type.S32
                    | PrimitiveType.S64 -> Tag.Type.S64
                    | PrimitiveType.U8 -> Tag.Type.U8
                    | PrimitiveType.U16 -> Tag.Type.U16
                    | PrimitiveType.U32 -> Tag.Type.U32
                    | PrimitiveType.U64 -> Tag.Type.U64
                    | PrimitiveType.F32 -> Tag.Type.F32
                    | PrimitiveType.F64 -> Tag.Type.F64
                    | PrimitiveType.Char16 -> Tag.Type.Char16
                    | PrimitiveType.Char32 -> Tag.Type.Char32
                    | PrimitiveType.Bool -> Tag.Type.Bool
                    | PrimitiveType.Unit -> Tag.Type.Unit
                bits1 tag dest
            | _ -> failwithf "TODO: Unsupported type %A" signature

        lengthEncodedVector buffer stream md.MethodSignatures <| fun signature dest ->
            vector index signature.ReturnTypes dest
            vector index signature.ParameterTypes dest

        let auxbuf = new MemoryStream()

        lengthEncodedData buffer stream <| fun dest ->
            vector moduleID md.Imports.ImportedModules dest

            lengthEncodedVector auxbuf dest md.Imports.ImportedTypes <| fun tdef dest ->
                index tdef.Module dest
                index tdef.TypeName dest
                bits1 tdef.TypeKind dest
                LEB128.uint tdef.TypeParameters dest
                if tdef.TypeParameters > 0u then failwith "TODO: Type imports with type parameters are not yet supported"

            lengthEncodedVector auxbuf dest md.Imports.ImportedFields <| fun fimport dest ->
                index fimport.FieldOwner dest
                index fimport.FieldName dest
                index fimport.FieldType dest

            lengthEncodedVector auxbuf dest md.Imports.ImportedMethods <| fun mimport dest ->
                index mimport.MethodOwner dest
                index mimport.MethodName dest
                LEB128.uint mimport.TypeParameters dest
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

        lengthEncodedVector buffer stream md.Data (fun data dest -> dest.Write(data.AsSpan()))

        lengthEncodedVector buffer stream md.Code <| fun code dest ->
            vector registerType code.RegisterTypes dest
            lengthEncodedVector auxbuf dest code.Instructions (instruction md.Endianness)

        match md.EntryPoint with
        | ValueNone -> LEB128.uint 0u stream
        | ValueSome main -> lengthEncodedData buffer stream (index main)

        // Debug info not yet supported.
        stream.WriteByte 0uy // length of data
    finally
        stream.Close()

let toFile (destination: FileInfo) md =
    if isNull destination then nullArg(nameof destination)
    toStream (destination.OpenWrite()) md

let toPath destination md = toFile (FileInfo destination) md
