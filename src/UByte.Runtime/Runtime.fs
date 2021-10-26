﻿module rec UByte.Interpreter.Runtime

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

open UByte.Format.Model

let inline isFlagSet flag value = value &&& flag = flag

let primitiveTypeSize ptype =
    match ptype with
    | PrimitiveType.Bool
    | PrimitiveType.U8
    | PrimitiveType.S8 -> 1
    | PrimitiveType.U16
    | PrimitiveType.S16
    | PrimitiveType.Char16 -> 2
    | PrimitiveType.U32
    | PrimitiveType.S32
    | PrimitiveType.F32
    | PrimitiveType.Char32 -> 4
    | PrimitiveType.U64
    | PrimitiveType.S64
    | PrimitiveType.F64 -> 8
    | PrimitiveType.UNative
    | PrimitiveType.SNative -> sizeof<unativeint>
    | PrimitiveType.Unit -> 0

[<Struct>]
type OffsetArray<'T> =
    val Start: int32
    val private items: 'T[]

    new (start, items) = { Start = start; items = items }
    new (items) = OffsetArray(0, items)
    new (length) = OffsetArray(Array.zeroCreate<'T> length)

    member this.Length = if Object.ReferenceEquals(this.items, null) then 0 else this.items.Length - this.Start

    member this.AsSpan() = Span<'T>(this.items).Slice this.Start

    member this.Slice(start: int32) = OffsetArray<'T>(this.Start + start, this.items)

    member this.CopyTo(destination: OffsetArray<'T>) = this.AsSpan().CopyTo(destination.AsSpan())

[<Struct>]
type RuntimeStruct = { RawData: OffsetArray<byte>; References: OffsetArray<RuntimeObject> }

type RuntimeStruct with
    static member Raw(size: int32) = { RawData = OffsetArray size; References = OffsetArray() }
    static member Object() =
        let o = OffsetArray 1
        o.AsSpan().[0] <- RuntimeObject.Null
        { RawData = OffsetArray(); References = OffsetArray 1 }

[<AutoOpen>]
module private RuntimeStruct =
    let private invalidDataLength kind =
        invalidOp("Cannot copy " + kind + " from source to destination as the destination is too small")

    let copyRuntimeValue (source: inref<RuntimeStruct>) (destination: inref<RuntimeStruct>) =
        if source.RawData.Length > destination.RawData.Length then invalidDataLength "bytes"
        if source.References.Length > destination.References.Length then invalidDataLength "references"
        if destination.RawData.Length > source.RawData.Length then source.RawData.AsSpan().Clear()
        if destination.References.Length > source.References.Length then source.RawData.AsSpan().Clear()
        source.RawData.CopyTo destination.RawData
        source.References.CopyTo destination.References

type RuntimeRegister =
    { RegisterValue: RuntimeStruct; RegisterType: AnyType }

    override this.ToString() =
        match this.RegisterType with
        | ValueType(ValueType.Primitive prim) ->
            match prim with
            | PrimitiveType.Bool -> sprintf "%b" (this.RegisterValue.ReadRaw<bool> 0)
            | PrimitiveType.U8 -> sprintf "%iuy" (this.RegisterValue.ReadRaw<uint8> 0)
            | PrimitiveType.S8 -> sprintf "%iy" (this.RegisterValue.ReadRaw<int8> 0)
            | PrimitiveType.U16 -> sprintf "%ius" (this.RegisterValue.ReadRaw<uint16> 0)
            | PrimitiveType.S16 -> sprintf "%is" (this.RegisterValue.ReadRaw<int16> 0)
            | PrimitiveType.Char16 -> string(this.RegisterValue.ReadRaw<char> 0)
            | PrimitiveType.U32 -> sprintf "%iu" (this.RegisterValue.ReadRaw<uint32> 0)
            | PrimitiveType.S32 -> string(this.RegisterValue.ReadRaw<int32> 0)
            | PrimitiveType.Char32 -> this.RegisterValue.ReadRaw<System.Text.Rune>(0).ToString()
            | PrimitiveType.U64 -> sprintf "%iUL" (this.RegisterValue.ReadRaw<uint32> 0)
            | PrimitiveType.S64 -> sprintf "%iL" (this.RegisterValue.ReadRaw<int32> 0)
            | PrimitiveType.UNative -> sprintf "%iun" (this.RegisterValue.ReadRaw<unativeint> 0)
            | PrimitiveType.SNative -> sprintf "%in" (this.RegisterValue.ReadRaw<nativeint> 0)
            | PrimitiveType.F32 -> sprintf "%A" (this.RegisterValue.ReadRaw<single> 0)
            | PrimitiveType.F64 -> string(this.RegisterValue.ReadRaw<double> 0)
            | PrimitiveType.Unit -> "()"
        | ReferenceType rtype ->
            match this.RegisterValue.ReadRef 0, rtype with
            | RuntimeObject.Null, _ -> "null"
            | RuntimeObject.TypeInstance(otype, _), _ -> otype.ToString()
            | RuntimeObject.Array chars, ReferenceType.Vector(ReferenceOrValueType.Value(ValueType.Primitive PrimitiveType.Char16)) ->
                let str = System.Text.StringBuilder chars.Length
                for i = 0 to chars.Length - 1 do str.Append(chars.[i].ReadRaw<char> 0) |> ignore
                str.ToString()
            | RuntimeObject.Array chars, ReferenceType.Vector(ReferenceOrValueType.Value(ValueType.Primitive PrimitiveType.Char32)) ->
                let str = System.Text.StringBuilder(2 * chars.Length)
                for i = 0 to chars.Length - 1 do str.Append(chars.[i].ReadRaw<System.Text.Rune>(0).ToString()) |> ignore
                str.ToString()
            | _, _ -> string rtype
        | atype -> string atype

type RuntimeRegister with
    static member Raw(size, vtype) =
        { RegisterValue = RuntimeStruct.Raw size
          RegisterType = AnyType.ValueType vtype }

    static member Object rtype =
        { RegisterValue = RuntimeStruct.Object()
          RegisterType = AnyType.ReferenceType rtype }

[<RequireQualifiedAccess>]
module private RuntimeRegister =
    let primitiveType register =
        match register.RegisterType with
        | AnyType.ValueType(ValueType.Primitive prim) -> prim
        | _ -> invalidArg (nameof register) "Cannot retrieve numeric value from a register not containing a primitive type"

[<RequireQualifiedAccess>]
module private Reinterpret =
    let inline (|F32|) value: uint32 =
        let mutable value = value
        Unsafe.As<single, uint32> &value

    let inline (|U32|) value: single =
        let mutable value = value
        Unsafe.As<uint32, single> &value

    let inline (|F64|) value: uint64 =
        let mutable value = value
        Unsafe.As<double, uint64> &value

    let inline (|U64|) value: double =
        let mutable value = value
        Unsafe.As<uint64, double> &value

[<Sealed>]
type RuntimeStackFrame
    (
        prev: RuntimeStackFrame voption,
        args: ImmutableArray<RuntimeRegister>,
        returns: ImmutableArray<RuntimeRegister>,
        blocks: ImmutableArray<CodeBlock>,
        method: RuntimeMethod
    )
    =
    let mutable bindex, iindex = 0, 0

    member _.ArgumentRegisters = args
    member _.InstructionIndex with get() = iindex and set i = iindex <- i
    member _.BlockIndex with get() = bindex and set i = bindex <- i
    member _.ReturnRegisters = returns
    member _.Code = blocks
    member _.CurrentMethod = method
    member _.Previous = prev

    member this.StackTrace =
        let trace = System.Text.StringBuilder()
        let mutable current = ValueSome this
        while current.IsSome do
            let current' = current.Value
            Printf.bprintf trace "  at %O" current'.CurrentMethod
            if not current'.CurrentMethod.IsExternal then
                Printf.bprintf trace "block %i instruction 0x%04X" current'.BlockIndex current'.InstructionIndex
            current <- current'.Previous
            if current.IsSome then trace.Append Environment.NewLine |> ignore
        trace.ToString()

type RuntimeException =
    inherit Exception

    val private frame: RuntimeStackFrame voption

    new (frame, message, inner: Exception) = { inherit Exception(message, inner); frame = frame }

    new (frame, message) = RuntimeException(frame, message, null)

    member this.CurrentFrame = this.frame

type MethodInvocationResult = ImmutableArray<RuntimeRegister>

type FieldAccessException (frame, message, field: RuntimeField) =
    inherit RuntimeException(frame, message)

    member _.Field = field

[<Sealed>]
type NullReferenceFieldAccessException (frame, message, field) = inherit FieldAccessException(frame, message, field)

[<Sealed>]
type RuntimeArray =
    val Length: int32
    val Data: byte[]
    val References: RuntimeObject[]

    new (rawDataSize, objectReferencesLength, length) =
        if length < 0 then raise(ArgumentOutOfRangeException(nameof length, length, "The length of an array cannot be negative"))
        { Length = length
          Data = Array.zeroCreate<byte> (Checked.(*) rawDataSize length)
          References = Array.zeroCreate<RuntimeObject> (Checked.(*) objectReferencesLength length) }

    new (stype: RuntimeTypeDefinition, length) =
        let layout = stype.Layout
        RuntimeArray(layout.RawDataSize, layout.ObjectReferencesLength, length)

    member private this.ElementLength arr = Array.length arr / this.Length

    member private this.ElementReferences i = OffsetArray<RuntimeObject>(i * this.ElementLength this.References, this.References)

    member this.Item
        with get(index: int32): RuntimeStruct =
            { RawData = OffsetArray<byte>(index * this.ElementLength this.Data, this.Data)
              References = this.ElementReferences index }
        and set index (value: RuntimeStruct) =
            value.RawData.AsSpan().CopyTo(Span(this.Data, index * this.ElementLength this.Data, value.RawData.Length))
            let rdestination = Span(this.References, index * this.ElementLength this.References, value.References.Length)
            value.References.AsSpan().CopyTo rdestination

[<RequireQualifiedAccess; NoComparison; ReferenceEquality>]
type RuntimeObject =
    | Null
    /// Represents a class instance or a boxed value type.
    | TypeInstance of otype: RuntimeTypeDefinition * fields: RuntimeStruct
    | Array of RuntimeArray

type RuntimeStruct with
    member this.ReadRaw<'T when 'T : struct and 'T :> System.ValueType and 'T : (new: unit -> 'T)> (index: int32): 'T =
        MemoryMarshal.Read<'T>(Span.op_Implicit(this.RawData.AsSpan()).Slice(index))

    member this.WriteRaw<'T when 'T : struct and 'T :> System.ValueType and 'T : (new: unit -> 'T)>(index, value: 'T) =
        let mutable value = value
        MemoryMarshal.Write<'T>(this.RawData.AsSpan().Slice(index), &value)

    member this.ReadRef index = this.References.AsSpan().[index]

    member this.WriteRef(index, o) = this.References.AsSpan().[index] <- o

[<Sealed>]
type MissingReturnInstructionException (frame, message) = inherit RuntimeException(frame, message)

[<Sealed>]
type InvalidComparisonException (message) = inherit RuntimeException(ValueNone, message)

[<RequireQualifiedAccess>]
module Interpreter =
    open UByte.Format.Model.InstructionSet

    let private copyRegisterValues (source: ImmutableArray<RuntimeRegister>) (dest: ImmutableArray<RuntimeRegister>) =
        if source.Length > dest.Length then failwith "TODO: Error, more source registers than destination registers" // TODO: Have validation of arguments lengths be the caller's problem.
        for i = 0 to dest.Length - 1 do copyRuntimeValue &source.[i].RegisterValue &dest.[i].RegisterValue

    /// Contains functions for retrieving numeric values stored in registers.
    [<RequireQualifiedAccess>]
    module private NumberValue =
        let inline private number register u8 s8 u16 s16 u32 s32 u64 s64 f32 f64 unative snative =
            let value = &register.RegisterValue
            match RuntimeRegister.primitiveType register with
            | PrimitiveType.Bool
            | PrimitiveType.U8 -> value.ReadRaw<uint8> 0 |> u8
            | PrimitiveType.S8 -> value.ReadRaw<int8> 0 |> s8
            | PrimitiveType.Char16
            | PrimitiveType.U16 -> value.ReadRaw<uint16> 0 |> u16
            | PrimitiveType.S16 -> value.ReadRaw<int16> 0 |> s16
            | PrimitiveType.Char32
            | PrimitiveType.U32 -> value.ReadRaw<uint32> 0 |> u32
            | PrimitiveType.S32 -> value.ReadRaw<int32> 0 |> s32
            | PrimitiveType.U64 -> value.ReadRaw<uint64> 0 |> u64
            | PrimitiveType.S64 -> value.ReadRaw<int64> 0 |> s64
            | PrimitiveType.F32 -> value.ReadRaw<single> 0 |> f32
            | PrimitiveType.F64 -> value.ReadRaw<double> 0 |> f64
            | PrimitiveType.UNative -> value.ReadRaw<unativeint> 0 |> unative
            | PrimitiveType.SNative -> value.ReadRaw<nativeint> 0 |> snative
            | PrimitiveType.Unit -> Unchecked.defaultof<_>

        let u8 register = number register id uint8 uint8 uint8 uint8 uint8 uint8 uint8 uint8 uint8 uint8 uint8
        let s8 register = number register int8 id int8 int8 int8 int8 int8 int8 int8 int8 int8 int8
        let u16 register = number register uint16 uint16 id uint16 uint16 uint16 uint16 uint16 uint16 uint16 uint16 uint16
        let s16 register = number register int16 int16 int16 id int16 int16 int16 int16 int16 int16 int16 int16
        let u32 register = number register uint32 uint32 uint32 uint32 id uint32 uint32 uint32 uint32 uint32 uint32 uint32
        let s32 register = number register int32 int32 int32 int32 int32 id int32 int32 int32 int32 int32 int32
        let u64 register = number register uint64 uint64 uint64 uint64 uint64 uint64 id uint64 uint64 uint64 uint64 uint64
        let s64 register = number register int64 int64 int64 int64 int64 int64 int64 id int64 int64 int64 int64

        let f32 register =
            number register float32 float32 float32 float32 float32 float32 float32 float32 id float32 float32 float32

        let f64 register = number register float float float float float float float float float id float float

        let snative register =
            number register nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint id

        let unative register =
            number register unativeint unativeint unativeint unativeint unativeint  unativeint  unativeint  unativeint unativeint unativeint id unativeint

    /// Contains functions for performing arithmetic on the values stored in registers.
    [<RequireQualifiedAccess>]
    module private Arithmetic =
        let private noUnitType() = raise(InvalidOperationException "Cannot use Unit type in an arithmetic operation")

        // Performs an operation on two integers and stores a result value, with no overflow checks.
        let inline private binop opu8 ops8 opu16 ops16 opu32 ops32 opu64 ops64 opunative opsnative opf32 opf64 vtype xreg yreg (destination: inref<RuntimeStruct>) =
            match vtype with
            | PrimitiveType.U8
            | PrimitiveType.Bool -> destination.WriteRaw<uint8>(0, opu8 (NumberValue.u8 xreg) (NumberValue.u8 yreg))
            | PrimitiveType.S8 -> destination.WriteRaw<int8>(0, ops8 (NumberValue.s8 xreg) (NumberValue.s8 yreg))
            | PrimitiveType.U16
            | PrimitiveType.Char16 -> destination.WriteRaw<uint16>(0, opu16 (NumberValue.u16 xreg) (NumberValue.u16 yreg))
            | PrimitiveType.S16 -> destination.WriteRaw<int16>(0, ops16 (NumberValue.s16 xreg) (NumberValue.s16 yreg))
            | PrimitiveType.U32
            | PrimitiveType.Char32 -> destination.WriteRaw<uint32>(0, opu32 (NumberValue.u32 xreg) (NumberValue.u32 yreg))
            | PrimitiveType.S32 -> destination.WriteRaw<int32>(0, ops32 (NumberValue.s32 xreg) (NumberValue.s32 yreg))
            | PrimitiveType.U64 -> destination.WriteRaw<uint64>(0, opu64 (NumberValue.u64 xreg) (NumberValue.u64 yreg))
            | PrimitiveType.S64 -> destination.WriteRaw<int64>(0, ops64 (NumberValue.s64 xreg) (NumberValue.s64 yreg))
            | PrimitiveType.UNative ->
                destination.WriteRaw<unativeint>(0, opunative (NumberValue.unative xreg) (NumberValue.unative yreg))
            | PrimitiveType.SNative ->
                destination.WriteRaw<nativeint>(0, opsnative (NumberValue.snative xreg) (NumberValue.snative yreg))
            | PrimitiveType.F32 -> destination.WriteRaw<single>(0, opf32 (NumberValue.f32 xreg) (NumberValue.f32 yreg))
            | PrimitiveType.F64 -> destination.WriteRaw<double>(0, opf64 (NumberValue.f64 xreg) (NumberValue.f64 yreg))
            | PrimitiveType.Unit -> noUnitType()

        let inline private unop opu8 ops8 opu16 ops16 opu32 ops32 opu64 ops64 opunative opsnative opf32 opf64 vtype register (destination: inref<RuntimeStruct>) =
            let value = &register.RegisterValue
            match vtype with
            | PrimitiveType.S8 -> destination.WriteRaw<int8>(0, ops8(value.ReadRaw<int8> 0))
            | PrimitiveType.U8
            | PrimitiveType.Bool -> destination.WriteRaw<uint8>(0, opu8(value.ReadRaw<uint8> 0))
            | PrimitiveType.S16 -> destination.WriteRaw<int16>(0, ops16(value.ReadRaw<int16> 0))
            | PrimitiveType.U16
            | PrimitiveType.Char16 -> destination.WriteRaw<uint16>(0, opu16(value.ReadRaw<uint16> 0))
            | PrimitiveType.S32 -> destination.WriteRaw<int32>(0, ops32(value.ReadRaw<int32> 0))
            | PrimitiveType.U32
            | PrimitiveType.Char32 -> destination.WriteRaw<uint32>(0, opu32(value.ReadRaw<uint32> 0))
            | PrimitiveType.S64 -> destination.WriteRaw<int64>(0, ops64(value.ReadRaw<int64> 0))
            | PrimitiveType.U64 -> destination.WriteRaw<uint64>(0, opu64(value.ReadRaw<uint64> 0))
            | PrimitiveType.SNative -> destination.WriteRaw<nativeint>(0, opsnative(value.ReadRaw<nativeint> 0))
            | PrimitiveType.UNative -> destination.WriteRaw<unativeint>(0, opunative(value.ReadRaw<unativeint> 0))
            | PrimitiveType.F32 -> destination.WriteRaw<single>(0, opf32(value.ReadRaw<single> 0))
            | PrimitiveType.F64 -> destination.WriteRaw<double>(0, opf64(value.ReadRaw<double> 0))
            | PrimitiveType.Unit -> noUnitType()

        let add vtype xreg yreg (destination: inref<_>) =
            binop (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) vtype xreg yreg &destination

        let sub vtype xreg yreg (destination: inref<_>) =
            binop (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) vtype xreg yreg &destination

        let mul vtype xreg yreg (destination: inref<_>) =
            binop (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) vtype xreg yreg &destination

        let inline private integerDivideOperation op x y =
            if y <> LanguagePrimitives.GenericZero
            then op x y
            else raise(NotImplementedException "Integer division that cannot throw on division by zero is not implemented")

        let div vtype xreg yreg (destination: inref<_>) =
            binop (integerDivideOperation (/)) (integerDivideOperation (/)) (integerDivideOperation (/)) (integerDivideOperation (/)) (integerDivideOperation (/)) (integerDivideOperation (/)) (integerDivideOperation (/)) (integerDivideOperation (/)) (integerDivideOperation (/)) (integerDivideOperation (/)) (/) (/) vtype xreg yreg &destination

        let rem vtype xreg yreg (destination: inref<_>) =
            binop (integerDivideOperation (%)) (integerDivideOperation (%)) (integerDivideOperation (%)) (integerDivideOperation (%)) (integerDivideOperation (%)) (integerDivideOperation (%)) (integerDivideOperation (%)) (integerDivideOperation (%)) (integerDivideOperation (%)) (integerDivideOperation (%)) (/) (/) vtype xreg yreg &destination

        let private bitf32 operation = fun (Reinterpret.F32 x) (Reinterpret.F32 y) -> Reinterpret.(|U32|) (operation x y)
        let private bitf64 operation = fun (Reinterpret.F64 x) (Reinterpret.F64 y) -> Reinterpret.(|U64|) (operation x y)

        // TODO: Fix, bitwise operations should require that floats be reinterpreted as the corresponding integer types.
        let ``and`` vtype xreg yreg (destination: inref<_>) =
            binop (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (bitf32 (&&&)) (bitf64 (&&&)) vtype xreg yreg &destination

        let ``or`` vtype xreg yreg (destination: inref<_>) =
            binop (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) (bitf32 (|||)) (bitf64 (|||)) vtype xreg yreg &destination

        //let ``not`` xreg yreg rreg = unop (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) xreg yreg rreg

        let xor vtype xreg yreg (destination: inref<_>) =
            binop (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (bitf32 (^^^)) (bitf64 (^^^)) vtype xreg yreg &destination

        let inline private oneop (op: _ -> _ -> _) = op LanguagePrimitives.GenericOne

        let inline private increment value = oneop (+) value

        let incr vtype register (destination: inref<_>) =
            unop increment increment increment increment increment increment increment increment increment increment increment increment vtype register &destination

        let inline private decrement value = oneop (-) value

        let decr vtype register (destination: inref<_>) =
            unop decrement decrement decrement decrement decrement decrement decrement decrement decrement decrement decrement decrement vtype register &destination

        [<RequireQualifiedAccess>]
        module Checked =
            open Microsoft.FSharp.Core.Operators.Checked

            let add vtype xreg yreg (destination: inref<_>) =
                binop (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) vtype xreg yreg &destination

            let sub vtype xreg yreg (destination: inref<_>) =
                binop (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) vtype xreg yreg &destination

            let mul vtype xreg yreg (destination: inref<_>) =
                binop (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) vtype xreg yreg &destination

            let inline private floatDivideOperation op x y =
                if y <> LanguagePrimitives.GenericZero
                then op x y
                else raise(DivideByZeroException())

            let div vtype xreg yreg (destination: inref<_>) =
                binop (/) (/) (/) (/) (/) (/) (/) (/) (/) (/) (floatDivideOperation (/)) (floatDivideOperation (/)) vtype xreg yreg &destination

            let rem vtype xreg yreg (destination: inref<_>) =
                binop (%) (%) (%) (%) (%) (%) (%) (%) (%) (%) (floatDivideOperation (%)) (floatDivideOperation (%)) vtype xreg yreg &destination

            let inline private increment value = oneop (+) value

            let incr vtype register (destination: inref<_>) =
                unop increment increment increment increment increment increment increment increment increment increment increment increment vtype register &destination

            let inline private decrement value = oneop (-) value

            let decr vtype register (destination: inref<_>) =
                unop decrement decrement decrement decrement decrement decrement decrement decrement decrement decrement decrement decrement vtype register &destination

    /// Contains functions for comparing the values stored in registers.
    [<RequireQualifiedAccess>]
    module private Compare =
        let inline private comparison s8 u8 s16 u16 s32 u32 s64 u64 snative unative f32 f64 obj xreg yreg =
            match xreg.RegisterType, yreg.RegisterType with
            | AnyType.ReferenceType _, AnyType.ReferenceType _ ->
                obj (xreg.RegisterValue.ReadRef 0) (yreg.RegisterValue.ReadRef 0)
            | AnyType.ReferenceType _, _
            | _, AnyType.ReferenceType _ ->
                failwith "TODO: Error for comparisons between objects and numeric values are prohibited"
            | AnyType.ValueType(ValueType.Defined _), _
            | _, AnyType.ValueType(ValueType.Defined _) ->
                raise(NotImplementedException "Comparisons of structs may be implemented in the future")
            | AnyType.SafePointer _, _
            | _, AnyType.SafePointer _ ->
                failwith "TODO: Error for comparisons of safe pointers are prohibited"
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.F64), _ ->
                f64 (xreg.RegisterValue.ReadRaw<float> 0) (NumberValue.f64 yreg)
            | _, AnyType.ValueType(ValueType.Primitive PrimitiveType.F64) ->
                f64 (NumberValue.f64 xreg) (yreg.RegisterValue.ReadRaw<float> 0)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.F32), _ ->
                f32 (xreg.RegisterValue.ReadRaw<float32> 0) (NumberValue.f32 yreg)
            | _, AnyType.ValueType(ValueType.Primitive PrimitiveType.F32) ->
                f32 (NumberValue.f32 xreg) (yreg.RegisterValue.ReadRaw<float32> 0)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.SNative), AnyType.ValueType(ValueType.Primitive PrimitiveType.UNative | ValueType.UnsafePointer _) ->
                let x = xreg.RegisterValue.ReadRaw<nativeint> 0
                if x < 0n
                then failwith "TODO: How to compare long and ulong?"
                else unative (unativeint x) (yreg.RegisterValue.ReadRaw<unativeint> 0)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.UNative | ValueType.UnsafePointer _), AnyType.ValueType(ValueType.Primitive PrimitiveType.SNative) ->
                let y = yreg.RegisterValue.ReadRaw<nativeint> 0
                if y < 0n
                then failwith "TODO: How to compare long and ulong?"
                else unative (xreg.RegisterValue.ReadRaw<unativeint> 0) (unativeint y)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.UNative | ValueType.UnsafePointer _), _ ->
                unative (xreg.RegisterValue.ReadRaw<unativeint> 0) (NumberValue.unative yreg)
            | _, AnyType.ValueType(ValueType.Primitive PrimitiveType.UNative | ValueType.UnsafePointer _) ->
                unative (NumberValue.unative xreg) (yreg.RegisterValue.ReadRaw<unativeint> 0)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.SNative), _ ->
                snative (xreg.RegisterValue.ReadRaw<nativeint> 0) (NumberValue.snative yreg)
            | _, AnyType.ValueType(ValueType.Primitive PrimitiveType.SNative) ->
                snative (NumberValue.snative xreg) (yreg.RegisterValue.ReadRaw<nativeint> 0)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.S64), AnyType.ValueType(ValueType.Primitive PrimitiveType.U64) ->
                let x = xreg.RegisterValue.ReadRaw<int64> 0
                if x < 0L
                then failwith "TODO: How to compare long and ulong?"
                else u64 (uint64 x) (yreg.RegisterValue.ReadRaw<uint64> 0)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.U64), AnyType.ValueType(ValueType.Primitive PrimitiveType.S64) ->
                let y = yreg.RegisterValue.ReadRaw<int64> 0
                if y < 0L
                then failwith "TODO: How to compare long and ulong?"
                else u64 (xreg.RegisterValue.ReadRaw<uint64> 0) (uint64 y)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.U64), _ ->
                u64 (xreg.RegisterValue.ReadRaw<uint64> 0) (NumberValue.u64 yreg)
            | _, AnyType.ValueType(ValueType.Primitive PrimitiveType.U64) ->
                u64 (NumberValue.u64 xreg) (yreg.RegisterValue.ReadRaw<uint64> 0)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.S64), _ ->
                s64 (xreg.RegisterValue.ReadRaw<int64> 0) (NumberValue.s64 yreg)
            | _, AnyType.ValueType(ValueType.Primitive PrimitiveType.S64) ->
                s64 (NumberValue.s64 xreg) (yreg.RegisterValue.ReadRaw<int64> 0)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.S32), AnyType.ValueType(ValueType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32))
            | AnyType.ValueType(ValueType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32)), AnyType.ValueType(ValueType.Primitive PrimitiveType.S32) ->
                s64 (NumberValue.s64 xreg) (NumberValue.s64 yreg)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.U32), _ ->
                u32 (xreg.RegisterValue.ReadRaw<uint32> 0) (NumberValue.u32 yreg)
            | _, AnyType.ValueType(ValueType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32)) ->
                u32 (NumberValue.u32 xreg) (yreg.RegisterValue.ReadRaw<uint32> 0)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.S32), _ ->
                s32 (xreg.RegisterValue.ReadRaw<int32> 0) (NumberValue.s32 yreg)
            | _, AnyType.ValueType(ValueType.Primitive PrimitiveType.S32) ->
                s32 (NumberValue.s32 xreg) (yreg.RegisterValue.ReadRaw<int32> 0)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.S16), AnyType.ValueType(ValueType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16))
            | AnyType.ValueType(ValueType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16)), AnyType.ValueType(ValueType.Primitive PrimitiveType.S16) ->
                s32 (NumberValue.s32 xreg) (NumberValue.s32 yreg)
            | AnyType.ValueType(ValueType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16)), _ ->
                u16 (xreg.RegisterValue.ReadRaw<uint16> 0) (NumberValue.u16 yreg)
            | _, AnyType.ValueType(ValueType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16)) ->
                u16 (NumberValue.u16 xreg) (yreg.RegisterValue.ReadRaw<uint16> 0)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.S16), _ ->
                s16 (xreg.RegisterValue.ReadRaw<int16> 0) (NumberValue.s16 yreg)
            | _, AnyType.ValueType(ValueType.Primitive PrimitiveType.S16) ->
                s16 (NumberValue.s16 xreg) (yreg.RegisterValue.ReadRaw<int16> 0)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.S8), AnyType.ValueType(ValueType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool))
            | AnyType.ValueType(ValueType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool)), AnyType.ValueType(ValueType.Primitive PrimitiveType.S8) ->
                s16 (NumberValue.s16 xreg) (NumberValue.s16 yreg)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.S8), _ ->
                s8 (xreg.RegisterValue.ReadRaw<int8> 0) (NumberValue.s8 yreg)
            | _, AnyType.ValueType(ValueType.Primitive PrimitiveType.S8) ->
                s8 (NumberValue.s8 xreg) (yreg.RegisterValue.ReadRaw<int8> 0)
            | AnyType.ValueType(ValueType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool)), _ ->
                u8 (xreg.RegisterValue.ReadRaw<uint8> 0) (NumberValue.u8 yreg)
            | _, AnyType.ValueType(ValueType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool)) ->
                u8 (NumberValue.u8 xreg) (yreg.RegisterValue.ReadRaw<uint8> 0)
            | AnyType.ValueType(ValueType.Primitive PrimitiveType.Unit), _
            | _, AnyType.ValueType(ValueType.Primitive PrimitiveType.Unit) ->
                true

        let isTrueValue register =
            let rvalue = &register.RegisterValue
            let mutable value, i = false, 0
            let bytes = rvalue.RawData.AsSpan()
            while not value && i < bytes.Length do
                value <- bytes.[i] <> 0uy
                i <- i + 1
            i <- 0
            let references = rvalue.References.AsSpan()
            while not value && i < references.Length do
                value <- references.[i] <> RuntimeObject.Null
                i <- i + 1
            value

        let inline isFalseValue register = not(isTrueValue register)

        let isLessThan xreg yreg = comparison (<) (<) (<) (<) (<) (<) (<) (<) (<) (<) (<) (<) (fun _ _ -> false) xreg yreg
        let isGreaterThan xreg yreg = comparison (>) (>) (>) (>) (>) (>) (>) (>) (>) (>) (>) (>) (fun _ _ -> false) xreg yreg
        let private refeq a b = Object.ReferenceEquals(a, b)
        let isEqual xreg yreg = comparison (=) (=) (=) (=) (=) (=) (=) (=) (=) (=) (=) (=) refeq xreg yreg
        let inline isNotEqual xreg yreg = not(isEqual xreg yreg)
        let isLessOrEqual xreg yreg = comparison (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) refeq xreg yreg
        let isGreaterOrEqual xreg yreg = comparison (>=) (>=) (>=) (>=) (>=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) refeq xreg yreg

    /// Contains functions for storing values into registers.
    [<RequireQualifiedAccess>]
    module private Constant =
        let inline private number vtype value (destination: inref<RuntimeStruct>) =
            match vtype with
            | PrimitiveType.U32 -> destination.WriteRaw<uint32>(0, Checked.uint32 value)
            | PrimitiveType.S32 -> destination.WriteRaw<int32>(0, Checked.int32 value)

        let uinteger vtype (value: uint32) (destination: inref<_>) = number vtype value &destination
        let sinteger vtype (value: int32) (destination: inref<_>) = number vtype value &destination

        let private noBooleanFloat() = invalidOp "Conversion of boolean value to float is not supported"

        let boolean vtype value (destination: inref<RuntimeStruct>) =
            match vtype with
            | PrimitiveType.Bool | PrimitiveType.S8 | PrimitiveType.U8 ->
                destination.WriteRaw<bool>(0, value)
            | PrimitiveType.F32 | PrimitiveType.F64 ->
                noBooleanFloat()

    let private (|ValidArithmeticFlags|) (flags: ArithmeticFlags) =
        if flags &&& (~~~ArithmeticFlags.ValidMask) <> ArithmeticFlags.None then
            failwithf "TODO: Bad arithmetic flags %A" flags
        flags

    [<Sealed>]
    type TemporaryRegisterPool () =
        let mutable data, dcount = Array.zeroCreate<byte> 64, 0
        let mutable references, rcount = Array.zeroCreate<RuntimeObject> 4, 0
        // TODO: Have pool for RuntimeRegister instances to avoid creating new ones all the time.
        let registers = ImmutableArray.CreateBuilder()

        static member private CreateOrReuseArray(size, array: byref<'T[]>, count: byref<_>) =
            let count' = Checked.(+) count size
            if count + size <= array.Length then
                let start = count
                count <- count'
                OffsetArray(start, array)
            else
                count <- 0
                array <- Array.zeroCreate(max count' (array.Length * 2))
                OffsetArray(Array.zeroCreate size)

        member _.Count = registers.Count

        member _.Item with get (Index index: TemporaryIndex) = registers.[Checked.int32 index]

        member _.Allocate(rtype, dsize, rlength) =
            let register =
                { RuntimeRegister.RegisterType = rtype
                  // TODO: Fix, if data or references array is reallocated then bad things
                  RegisterValue =
                    { RuntimeStruct.RawData = TemporaryRegisterPool.CreateOrReuseArray(dsize, &data, &dcount)
                      References = TemporaryRegisterPool.CreateOrReuseArray(rlength, &references, &rcount) } }
            registers.Add register
            register

        member _.Clear() =
            registers.Clear()
            Span(data).Clear()
            Span(references).Clear()
            dcount <- 0
            rcount <- 0

    let interpret returns arguments (entrypoint: RuntimeMethod) =
        let frame: RuntimeStackFrame voption ref = ref ValueNone
        let temporaries = TemporaryRegisterPool()
        let mutable runExternalCode: (RuntimeStackFrame voption ref -> unit) voption = ValueNone
        let mutable ex = ValueNone

#if DEBUG
        let invoke returns (arguments: ImmutableArray<_>) (method: RuntimeMethod) =
#else
        let inline invoke returns (arguments: ImmutableArray<_>) (method: RuntimeMethod) =
#endif
            method.SetupStackFrame(returns, frame, &runExternalCode)
            let arguments' = frame.Value.Value.ArgumentRegisters
            if arguments.Length <> arguments'.Length then
                failwithf "TODO: Error for argument array lengths do not match, expected %i got %i" arguments'.Length arguments.Length
            temporaries.Clear()
            copyRegisterValues arguments arguments'

        invoke returns arguments entrypoint

        let inline cont() =
            match frame.Value with
            | ValueSome frame'-> frame'.BlockIndex < frame'.Code.Length
            | ValueNone -> false

        let lookupRegisterAt (RegisterIndex.Index index) =
            let tcount = uint32 temporaries.Count
            if index < tcount then
                temporaries.[Index index]
            else
                let frame' = frame.Value.Value
                let index' = index + tcount
                let acount = uint32 frame'.ArgumentRegisters.Length
                if index' < acount then
                    frame'.ArgumentRegisters.[Checked.int32 index]
                else
                    failwith "TODO: Lookup local registers"

        while cont() do
            let frame' = frame.Value.Value

            let inline (|Register|) rindex = lookupRegisterAt rindex
            let inline (|Method|) mindex: RuntimeMethod = frame'.CurrentMethod.Module.InitializeMethod mindex
            let inline (|Field|) findex: RuntimeField = frame'.CurrentMethod.Module.InitializeField findex
            let inline (|TypeSignature|) tindex: AnyType = frame'.CurrentMethod.Module.TypeSignatureAt tindex
            let inline (|Data|) dindex: ImmutableArray<_> = frame'.CurrentMethod.Module.DataAt dindex
            let inline (|BranchTarget|) (target: BlockOffset) = Checked.(+) frame'.BlockIndex target

#if DEBUG
            let inline fieldAccessInstruction field object access =
#else
            let fieldAccessInstruction field object access =
#endif
                if object.RegisterValue.References.Length = 0 then
                    failwith "TODO: How to handle field access for struct?"
                match object.RegisterValue.ReadRef 0 with
                | RuntimeObject.TypeInstance(otype, data) ->
                    let struct(dindex, rindex) = otype.Layout.FieldIndices.[field]
                    access
                        { RuntimeStruct.RawData = OffsetArray(data.RawData.AsSpan().Slice(dindex, field.DataSize).ToArray())
                          References = OffsetArray(data.References.AsSpan().Slice(rindex, field.ReferencesLength).ToArray()) }
                | RuntimeObject.Null ->
                    raise(NullReferenceException(sprintf "Attempted to access the field %O with a null object reference" field))
                | RuntimeObject.Array _ ->
                    failwith "TODO: Error when attempted to access object field using reference to array"

#if DEBUG
            let arrayAccessInstruction array index access =
#else
            let inline arrayAccessInstruction array index access =
#endif
                let index' = NumberValue.s32 index
                match array.RegisterValue.ReadRef 0 with
                | RuntimeObject.Array array' -> access array' index'
                | RuntimeObject.TypeInstance(otype, _) ->
                    invalidOp("Cannot access array item with an object reference of type " + otype.ToString())
                | RuntimeObject.Null ->
                    raise(NullReferenceException "Cannot access an array element with a null array reference")

            let inline allocateRegisterPrimitive rtype =
                temporaries.Allocate(AnyType.ValueType(ValueType.Primitive rtype), primitiveTypeSize rtype, 0)

            match ex with
            | ValueSome e ->
                raise(NotImplementedException("TODO: Implement exception handling: " + Environment.NewLine + frame'.StackTrace, e))
            | ValueNone -> ()

            try
                match frame'.Code.[frame'.BlockIndex].Instructions.[frame'.InstructionIndex] with
                // TODO: Create common helper function for arithmetic operations, and don't use inref<_> since RuntimeRegister can be used directly
                | Add(ValidArithmeticFlags flags, vtype, Register x, Register y) ->
                    let destination = allocateRegisterPrimitive vtype
                    if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                    then Arithmetic.Checked.add vtype x y &destination.RegisterValue
                    else Arithmetic.add vtype x y &destination.RegisterValue
                | Sub(ValidArithmeticFlags flags, vtype, Register x, Register y) ->
                    let destination = allocateRegisterPrimitive vtype
                    if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                    then Arithmetic.Checked.sub vtype x y &destination.RegisterValue
                    else Arithmetic.sub vtype x y &destination.RegisterValue
                | Mul(ValidArithmeticFlags flags, vtype, Register x, Register y) ->
                    let destination = allocateRegisterPrimitive vtype
                    if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                    then Arithmetic.Checked.mul vtype x y &destination.RegisterValue
                    else Arithmetic.mul vtype x y &destination.RegisterValue
                | Div(ValidArithmeticFlags flags, vtype, Register x, Register y) ->
                    let destination = allocateRegisterPrimitive vtype
                    if isFlagSet ArithmeticFlags.ThrowOnDivideByZero flags
                    then Arithmetic.Checked.div vtype x y &destination.RegisterValue
                    else Arithmetic.div vtype x y &destination.RegisterValue
                | Rem(ValidArithmeticFlags flags, vtype, Register x, Register y) ->
                    let destination = allocateRegisterPrimitive vtype
                    if isFlagSet ArithmeticFlags.ThrowOnDivideByZero flags
                    then Arithmetic.Checked.rem vtype x y &destination.RegisterValue
                    else Arithmetic.rem vtype x y &destination.RegisterValue
                | Incr(ValidArithmeticFlags flags, vtype, Register register) ->
                    let destination = allocateRegisterPrimitive vtype
                    if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                    then Arithmetic.Checked.incr vtype register &destination.RegisterValue
                    else Arithmetic.incr vtype register &destination.RegisterValue
                | Decr(ValidArithmeticFlags flags, vtype, Register register) ->
                    let destination = allocateRegisterPrimitive vtype
                    if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                    then Arithmetic.Checked.decr vtype register &destination.RegisterValue
                    else Arithmetic.decr vtype register &destination.RegisterValue
                // TODO: Handle bitwise operatiors
                // TODO: Should exception be thrown if Constant integer overflows?
                | Const_u(vtype, value) ->
                    let destination = allocateRegisterPrimitive vtype
                    Constant.uinteger vtype value &destination.RegisterValue
                | Const_s(vtype, value) ->
                    let destination = allocateRegisterPrimitive vtype
                    Constant.sinteger vtype value &destination.RegisterValue
                | Const_true vtype ->
                    let destination = allocateRegisterPrimitive vtype
                    Constant.boolean vtype true &destination.RegisterValue
                | Const_false vtype | Const_zero vtype ->
                    let destination = allocateRegisterPrimitive vtype
                    Constant.boolean vtype true &destination.RegisterValue
                //| Const_f32 value ->
                //| Const_f64 value ->
                | Nop -> ()

                match runExternalCode with
                | ValueNone -> ()
                | ValueSome run ->
                    run frame
                    runExternalCode <- ValueNone
            with
            | e -> ex <- ValueSome e

            frame'.InstructionIndex <- Checked.(+) frame'.InstructionIndex 1
            if frame'.InstructionIndex = frame'.Code.[frame'.BlockIndex].Instructions.Length then
                frame'.InstructionIndex <- 0
                frame'.BlockIndex <- Checked.(+) frame'.BlockIndex 1
                // TODO: Assign local registers
                temporaries.Clear()

        match frame.contents with
        | ValueNone -> ()
        | ValueSome _ -> raise(MissingReturnInstructionException(frame.contents, "Reached unexpected end of code"))

[<Sealed>]
type InvalidConstructorException (method: RuntimeMethod, frame, message) =
    inherit RuntimeException(frame, message)

    member _.Method = method

[<RequireQualifiedAccess>]
module ExternalCode =
    [<Literal>]
    let private InternalCall = "runmdl"

    let private lookup = Dictionary<struct(string * string), RuntimeStackFrame -> unit>()

    let private println (frame: RuntimeStackFrame) =
        match frame.ArgumentRegisters.[0].RegisterValue.ReadRef 0 with
        | RuntimeObject.Null -> stdout.WriteLine()
        | RuntimeObject.Array chars ->
            for i = 0 to chars.Length - 1 do
                let c = chars.[i].ReadRaw<System.Text.Rune> 0
                stdout.Write(c.ToString())
            stdout.WriteLine()
        | _ ->
            failwith "TODO: How to print some other thing"

    do lookup.[(InternalCall, "testhelperprintln")] <- println
    do lookup.[(InternalCall, "break")] <- fun _ -> System.Diagnostics.Debugger.Launch() |> ignore

    let call library name =
        match lookup.TryGetValue(struct(library, name)) with
        | true, call' -> call'
        | false, _ -> fun _ -> failwithf "TODO: Handle external calls to %s in %s" library name

[<Sealed>]
type AbstractMethodCallException (method: RuntimeMethod, frame, message) =
    inherit RuntimeException(frame, message)

    member _.Method = method

[<Sealed>]
type RuntimeMethod (rmodule: RuntimeModule, index: MethodIndex, method: Method) =
    let { Method.MethodFlags = flags; Body = body } = method
    do if isFlagSet (MethodFlags.Virtual ||| MethodFlags.Constructor) flags then
        failwith "TODO: Error for constructor cannot be marked virtual"

    member _.Module: RuntimeModule = rmodule

    member val Name = rmodule.IdentifierAt method.MethodName

    member val Visibility = method.MethodVisibility

    member val DeclaringType = rmodule.InitializeType method.MethodOwner

    member val Signature = rmodule.MethodSignatureAt method.Signature

    member _.IsInstance = isFlagSet MethodFlags.Instance flags
    member _.IsConstructor = isFlagSet MethodFlags.ConstructorMask flags
    member _.IsVirtual = isFlagSet MethodFlags.Virtual flags
    member _.IsExternal =
        match method.Body with
        | MethodBody.External _ -> true
        | _ -> false

    member private _.CreateRegister rtype =
        match rmodule.TypeSignatureAt rtype with
        | ValueType vt ->
            match vt with
            | ValueType.Primitive PrimitiveType.S8 
            | ValueType.Primitive PrimitiveType.Bool
            | ValueType.Primitive PrimitiveType.U8 -> fun() -> RuntimeRegister.Raw(1, vt)
            | ValueType.Primitive PrimitiveType.S16
            | ValueType.Primitive PrimitiveType.U16
            | ValueType.Primitive PrimitiveType.Char16 -> fun() -> RuntimeRegister.Raw(2, vt)
            | ValueType.Primitive PrimitiveType.S32
            | ValueType.Primitive PrimitiveType.U32
            | ValueType.Primitive PrimitiveType.Char32 -> fun() -> RuntimeRegister.Raw(4, vt)
            | ValueType.Primitive PrimitiveType.S64
            | ValueType.Primitive PrimitiveType.U64
            | ValueType.Primitive PrimitiveType.F32
            | ValueType.Primitive PrimitiveType.F64 -> fun() -> RuntimeRegister.Raw(9, vt)
            | ValueType.Primitive PrimitiveType.SNative
            | ValueType.Primitive PrimitiveType.UNative
            | ValueType.UnsafePointer _ -> fun() -> RuntimeRegister.Raw(sizeof<nativeint>, vt)
            | ValueType.Primitive PrimitiveType.Unit -> fun() -> failwith "TODO: Prevent usage of Unit in register types."
            | ValueType.Defined _ -> failwith "TODO: Add support for registers containing structs"
        | ReferenceType rt -> fun() -> RuntimeRegister.Object rt
        | SafePointer _ -> failwithf "TODO: Safe pointers in registers not yet supported"

    member this.CreateArgumentRegisters() =
        let { MethodSignature.ParameterTypes = atypes } = rmodule.MethodSignatureAt method.Signature
        let mutable registers = Array.zeroCreate atypes.Length

        for i = 0 to registers.Length - 1 do
            registers.[i] <- this.CreateRegister atypes.[i] ()

        Unsafe.As<RuntimeRegister[], ImmutableArray<RuntimeRegister>> &registers

    member this.SetupStackFrame(returns, frame: _ ref, runExternalCode: outref<_ voption>) =
        match body with
        | MethodBody.Defined codei ->
            let code = rmodule.CodeAt codei
            let args = this.CreateArgumentRegisters()
            frame.contents <- ValueSome(RuntimeStackFrame(frame.contents, args, returns, code.Blocks, this))
        | MethodBody.Abstract ->
            if not this.IsVirtual then failwith "TODO: Error for abstract method must be virtual"

            AbstractMethodCallException (
                this,
                frame.contents,
                sprintf "Cannot directly call %O, use the call.virt instruction and related instructions instead" this
            )
            |> raise
        | MethodBody.External(library, efunction) ->
            let library' = this.Module.IdentifierAt library
            let efunction' = this.Module.IdentifierAt efunction
            let args = this.CreateArgumentRegisters()
            let frame' = RuntimeStackFrame(frame.contents, args, returns, ImmutableArray.Empty, this)
            frame.contents <- ValueSome frame'
            runExternalCode <- ValueSome <| fun frame'' ->
                ExternalCode.call library' efunction' frame''.contents.Value
                frame''.contents <- frame'.Previous

    override this.ToString() =
        let name = this.Name
        let t = System.Text.StringBuilder(this.DeclaringType.ToString()).Append('.')

        if String.IsNullOrEmpty name then
            let (Index i) = index
            t.Append('<')
                .Append(if this.IsConstructor then "constructor" else "method")
                .Append('-')
                .Append(i)
                .Append('>')
        else
            t.Append name
        |> ignore

        t.ToString()

[<Sealed>]
type RuntimeField (rmodule: RuntimeModule, field: Field, size) =
    let struct(dsize, rlength) = size
    let { Field.FieldName = namei; FieldFlags = flags } = field

    do
        if isFlagSet FieldFlags.Static field.FieldFlags then raise(NotSupportedException "Static fields are not yet supported")

    member _.Module = rmodule
    member _.Name = rmodule.IdentifierAt namei
    member _.DataSize: int32 = dsize
    member _.ReferencesLength: int32 = rlength

    member _.IsMutable = isFlagSet FieldFlags.Mutable flags
    member _.IsStatic = isFlagSet FieldFlags.Static flags

    member val DeclaringType: RuntimeTypeDefinition = rmodule.InitializeType field.FieldOwner

    member val FieldType = rmodule.TypeSignatureAt field.FieldType

    /// If the field is not marked as mutable, prevents modification of the field value outside of a constructor or type initializer.
    member this.CheckMutate(frame: RuntimeStackFrame) =
        if not this.IsMutable && not frame.CurrentMethod.IsConstructor then
            FieldAccessException (
                ValueSome frame,
                "Attempted to modify read-only field outside of constructor or type initializer",
                this
            )
            |> raise

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type RuntimeTypeLayout =
    { Fields: ImmutableArray<RuntimeField>
      FieldIndices: IReadOnlyDictionary<RuntimeField, struct(int32 * int32)>
      RawDataSize: int32
      ObjectReferencesLength: int32 }

[<Sealed>]
type RecursiveInheritanceException (message, t: RuntimeTypeDefinition) =
    inherit RuntimeException(ValueNone, message)
    member _.Type = t

[<NoComparison; NoEquality>]
type InheritedTypeLayout =
    { InheritedFieldCount: int32
      InheritedDataSize: int32
      InheritedReferencesLength: int32 }

[<Sealed>]
type RuntimeTypeDefinition (rm: RuntimeModule, t: TypeDefinition) as rt =
    static let emptyTypeLayout =
        lazy
            { RuntimeTypeLayout.Fields = ImmutableArray.Empty
              FieldIndices = ImmutableDictionary.Empty
              RawDataSize = 0
              ObjectReferencesLength = 0 }

    static let emptyInheritedTypes =
        lazy
            ImmutableArray<RuntimeTypeDefinition>.Empty,
            { InheritedFieldCount = 0; InheritedDataSize = 0; InheritedReferencesLength = 0 }

    static let emptyMethodOverrides = lazy(ImmutableDictionary.Empty :> IReadOnlyDictionary<_, _>)

    let findInheritedTypes =
        let { TypeDefinition.InheritedTypes = indices } = t
        if not indices.IsDefaultOrEmpty then
            lazy
                let mutable supers = Array.zeroCreate indices.Length
                let mutable inheritedFieldCount, inheritedDataSize, inheritedReferencesLength = 0, 0, 0
                for i = 0 to indices.Length - 1 do
                    let inherited = rm.InitializeType indices.[i]
                    supers.[i] <- inherited
                    if inherited = rt then
                        raise(RecursiveInheritanceException(sprintf "The type %O is not allowed to inherit from itself" rt, rt))
                    inheritedFieldCount <- Checked.(+) inheritedFieldCount inherited.Layout.Fields.Length
                    inheritedDataSize <- Checked.(+) inheritedDataSize inherited.Layout.RawDataSize
                    inheritedReferencesLength <- Checked.(+) inheritedReferencesLength inherited.Layout.ObjectReferencesLength
                Unsafe.As<RuntimeTypeDefinition[], ImmutableArray<RuntimeTypeDefinition>> &supers,
                { InheritedFieldCount = inheritedFieldCount
                  InheritedDataSize = inheritedDataSize
                  InheritedReferencesLength = inheritedReferencesLength }
        else
            emptyInheritedTypes

    // TODO: Calculate layout for static fields.
    let layout =
        let { TypeDefinition.Fields = fields } = t
        if not(t.InheritedTypes.IsDefaultOrEmpty && fields.IsDefaultOrEmpty) then
            // TODO: How to deal with the "Deadly Diamond of Death"
            // TODO: Update format to allow specifying of HOW fields are inherited (e.g. a flag before each type index).
            // TODO: In the future, check for recursion by ensuring that the list does not contain the current type
            lazy
                let (Lazy(inherited, inheritedTypeLayout)) = findInheritedTypes
                let fields' = ImmutableArray.CreateBuilder(inheritedTypeLayout.InheritedFieldCount + fields.Length)
                let fieldIndexLookup = Dictionary fields'.Capacity

                for super in inherited do
                    let superTypeLayout = super.Layout
                    fields'.AddRange superTypeLayout.Fields
                    for KeyValue(inheritedTypeField, inheritedFieldIndex) in superTypeLayout.FieldIndices do
                        fieldIndexLookup.Add(inheritedTypeField, inheritedFieldIndex)

                let mutable { InheritedDataSize = sumDataSize; InheritedReferencesLength = sumReferencesLength } =
                    inheritedTypeLayout

                for fieldi in fields do
                    let field = rm.InitializeField fieldi
                    if not field.IsStatic then
                        let dsize, rlen = field.DataSize, field.ReferencesLength
                        fields'.Add field
                        fieldIndexLookup.Add(field, struct(sumDataSize, sumReferencesLength))
                        sumDataSize <- sumDataSize + dsize
                        sumReferencesLength <- sumReferencesLength + rlen

                { RuntimeTypeLayout.Fields = fields'.ToImmutable()
                  FieldIndices = fieldIndexLookup :> IReadOnlyDictionary<_, _>
                  RawDataSize = sumDataSize
                  ObjectReferencesLength = sumReferencesLength }
        else
            emptyTypeLayout

    //let globals: RuntimeStruct

    let methodis = t.Methods

    let vtable =
        let { TypeDefinition.VTable = overrides } = t
        if not overrides.IsDefaultOrEmpty then
            lazy
                let lookup = Dictionary overrides.Length
                let inline (|Method|) mindex = rm.InitializeMethod mindex
                for { MethodOverride.Declaration = Method decl; Implementation = Method impl } in overrides do
                    // TODO: Check that owner of decl is an inherited type
                    // TODO: Check that impl is owned by this type
                    // TODO: Check that decl and impl are different
                    lookup.Add(decl, impl)
                System.Collections.ObjectModel.ReadOnlyDictionary lookup :> IReadOnlyDictionary<_, _>
        else
            emptyMethodOverrides

    member _.Module = rm

    member _.InheritedTypes = fst findInheritedTypes.Value

    member _.Layout: RuntimeTypeLayout = layout.Value

    member _.VTable: IReadOnlyDictionary<RuntimeMethod, RuntimeMethod> = vtable.Value

    member val Name = rm.IdentifierAt t.TypeName

    member val Namespace = rm.NamespaceAt t.TypeNamespace

    member _.FindMethod name = // TODO: Figure out if methods defined in inherited class(es)
        let mutable result, i = ValueNone, 0

        while i < methodis.Length && result.IsNone do
            let m = rm.InitializeMethod methodis.[i]
            if m.Visibility <= VisibilityFlags.Public && m.Name = name then
                result <- ValueSome m
            i <- Checked.(+) i 1

        match result with
        | ValueSome m -> m
        | ValueNone -> failwithf "TODO: Method not found %s" name

    member _.InitializeObjectFields(): RuntimeStruct =
        let layout' = layout.Value
        { RuntimeStruct.RawData = OffsetArray layout'.RawDataSize
          References = OffsetArray layout'.ObjectReferencesLength }

    override this.ToString() =
        System.Text.StringBuilder(this.Module.ToString())
            .Append(if this.Namespace.Length > 0 then "::" + this.Namespace else String.Empty)
            .Append("::")
            .Append(this.Name)
            .ToString()

let createIndexedLookup (count: int32) initializer =
    let lookup = Dictionary<Index<_>, _> count
    fun i ->
        match lookup.TryGetValue i with
        | true, existing -> existing
        | false, _ ->
            let value = initializer i
            lookup.Add(i, value)
            value

let createDefinitionOrImportLookup definedCount importCount definedInitializer importInitializer =
    createIndexedLookup (definedCount + importCount) <| fun (Index i as index) ->
        if i < uint32 importCount
        then importInitializer (Checked.int32 i) index
        else definedInitializer (Checked.int32 i - importCount) index

[<Sealed>]
type MissingEntryPointException (m: RuntimeModule, message: string) =
    inherit RuntimeException(ValueNone, message)

    member _.Module = m

[<Sealed>]
type TypeNotFoundException (m: RuntimeModule, typeNamespace, typeName, message: string) =
    inherit RuntimeException(ValueNone, message)

    member _.TypeNamespace: string = typeNamespace
    member _.TypeName: string = typeName
    member _.Module = m

//type State // TODO: Keep track of format version supported by the runtime.

[<Sealed>]
type RuntimeModule (m: Module, moduleImportResolver: ModuleIdentifier -> RuntimeModule) as rm =
    // TODO: Check if the runtime version matches the module's format version.

    let importedModuleLookup =
        let imports = m.Imports.ImportedModules
        createIndexedLookup imports.Length <| fun (Index i) ->
            if i = 0u
            then rm
            else moduleImportResolver imports.[Checked.int32 i - 1]

    let typeDefinitionLookup: TypeDefinitionIndex -> _ =
        let imports = m.Imports.ImportedTypes
        let definitions = m.Definitions.DefinedTypes
        createDefinitionOrImportLookup definitions.Length imports.Length
            (fun i _ -> RuntimeTypeDefinition(rm, definitions.[i]))
            (fun i _ ->
                let t = imports.[Checked.int32 i]
                let owner = importedModuleLookup t.Module
                owner.FindType(rm.NamespaceAt t.TypeNamespace, rm.IdentifierAt t.TypeName))

    let definedMethodLookup: MethodIndex -> RuntimeMethod =
        let imports = m.Imports.ImportedMethods
        let definitions = m.Definitions.DefinedMethods
        createDefinitionOrImportLookup definitions.Length imports.Length
            (fun i index -> RuntimeMethod(rm, index, definitions.[i]))
            (fun i _ ->
                let m = imports.[Checked.int32 i]
                let owner = typeDefinitionLookup m.MethodOwner
                owner.FindMethod(rm.IdentifierAt m.MethodName))

    // TODO: Account for imported fields when resolving indices.
    let definedFieldLookup =
        let owners = m.Definitions.DefinedTypes
        let fields = m.Definitions.DefinedFields
        createIndexedLookup fields.Length <| fun (Index i as i') ->
            let f = fields.[Checked.int32 i]
            let (Index owner) = f.FieldOwner
            //let n = owners.[Checked.int32 owner].Fields.IndexOf i' // Won't work for inherited types
            RuntimeField(rm, f, rm.CalculateTypeSize(rm.TypeSignatureAt f.FieldType))

    let typeNameLookup = Dictionary()

    member val Name = m.Header.Module.ModuleName.ToString()
    member val Version = m.Header.Module.Version

    member _.IdentifierAt(Index i: IdentifierIndex) = m.Identifiers.Identifiers.[Checked.int32 i]

    member this.NamespaceAt(Index i: NamespaceIndex): string =
        m.Namespaces.[Checked.int32 i] |> Seq.map this.IdentifierAt |> String.concat "::" // TODO: Cache namespaces

    member _.TypeSignatureAt(Index i: TypeSignatureIndex) = m.TypeSignatures.[Checked.int32 i]

    member _.MethodSignatureAt(Index i: MethodSignatureIndex) = m.MethodSignatures.[Checked.int32 i]

    member _.DataAt(Index i: DataIndex) = m.Data.[Checked.int32 i]

    member _.CodeAt(Index i: CodeIndex): Code = m.Code.[Checked.int32 i]

    member _.InitializeMethod i = definedMethodLookup i

    member _.InitializeField i = definedFieldLookup i

    member _.InitializeType i = typeDefinitionLookup i

    /// Finds the first type defined in this module with the specified name.
    member this.FindType(typeNamespace: string, typeName: string): RuntimeTypeDefinition =
        let key = struct(typeNamespace, typeName)
        match typeNameLookup.TryGetValue key with
        | false, _ ->
            let types = m.Definitions.DefinedTypes
            let mutable result, i = ValueNone, m.Imports.ImportedTypes.Length

            while i < types.Length && result.IsNone do
                let t = types.[i]

                if
                    t.TypeVisibility <= VisibilityFlags.Public &&
                    this.NamespaceAt t.TypeNamespace = typeNamespace &&
                    this.IdentifierAt t.TypeName = typeName
                then
                    let tindex = TypeDefinitionIndex.Index(Checked.uint32 i)
                    let init = typeDefinitionLookup tindex
                    typeNameLookup.[key] <- init
                    result <- ValueSome init

                i <- Checked.(+) 1 i

            match result with
            | ValueSome t -> t
            | ValueNone ->
                TypeNotFoundException (
                    this,
                    typeNamespace,
                    typeName, sprintf "Unable to find type %s %s" typeNamespace typeName
                )
                |> raise
        | true, existing -> existing

    member _.CalculateTypeSize(t: AnyType): struct(Int32 * int32) =
        match t with
        | ValueType vt ->
            match vt with
            | ValueType.Primitive prim -> struct(primitiveTypeSize prim, 0)
            | ValueType.UnsafePointer _ -> struct(sizeof<unativeint>, 0)
            | ValueType.Defined _ -> failwith "TODO: Struct fields are currently not yet supported"
        | ReferenceType _ ->
            struct(0, 1)
        | SafePointer _ ->
            failwith "TODO: Error for fields cannot contain safe pointers"

    member this.InvokeEntryPoint(argv: string[]) =
        match m.EntryPoint with
        | ValueSome ei ->
            let main = this.InitializeMethod ei

            if main.Module <> this then failwith "TODO: Error for entry point method must be defined in this module"

            let signature = main.Signature

            let arguments =
                match signature.ParameterTypes.Length with
                | 0 -> ImmutableArray.Empty
                | 1 ->
                    match this.TypeSignatureAt signature.ParameterTypes.[0] with
                    | AnyType.ReferenceType(ReferenceType.Vector tstring as argt) ->
                        match tstring with
                        | ReferenceOrValueType.Reference(ReferenceType.Vector tchar) ->
                            let inline characterArrayArguments convert =
                                let argv' = RuntimeArray(0, 1, argv.Length)
                                for i = 0 to argv.Length - 1 do
                                    let arg = RuntimeStruct.Object()
                                    arg.WriteRef(0, RuntimeObject.Array(convert argv.[i]))
                                    argv'.[i] <- arg
                                let argv'' = RuntimeRegister.Object argt
                                argv''.RegisterValue.WriteRef(0, RuntimeObject.Array argv')
                                ImmutableArray.Create argv''

                            match tchar with // TODO: Create a Char8 type for UTF-8 strings
                            | ReferenceOrValueType.Value(ValueType.Primitive PrimitiveType.Char16) ->
                                characterArrayArguments <| fun arg ->
                                    let arg' = RuntimeArray(2, 0, arg.Length)
                                    for i = 0 to arg.Length - 1 do
                                        let c = RuntimeStruct.Raw 2
                                        c.WriteRaw(0, uint16 arg.[i])
                                        arg'.[i] <- c
                                    arg'
                            | ReferenceOrValueType.Value(ValueType.Primitive PrimitiveType.Char32) ->
                                let buffer = List()
                                characterArrayArguments <| fun arg ->
                                    buffer.Clear()
                                    for cu in arg.EnumerateRunes() do buffer.Add(uint32 cu.Value)
                                    let arg' = RuntimeArray(4, 0, buffer.Count)
                                    for i = 0 to buffer.Count - 1 do
                                        let c = RuntimeStruct.Raw 4
                                        c.WriteRaw(0, buffer.[i])
                                        arg'.[i] <- c
                                    arg'
                            | bad -> failwithf "TODO: Invalid character type %A" bad
                        | bad -> failwithf "TODO: Invalid string type %A" bad
                    | bad -> failwithf "TODO: Error for invalid entrypoint argument type %A" bad
                | _ -> failwith "TODO: Error for invalid number of arguments for entrypoint"

            let result = RuntimeRegister.Raw(4, ValueType.Primitive PrimitiveType.S32)
            Interpreter.interpret (ImmutableArray.Create result) arguments main
            result.RegisterValue.ReadRaw<int32> 0
        | ValueNone -> raise(MissingEntryPointException(this, "The entry point method of the module is not defined"))

    override this.ToString() = sprintf "(%s, v%O)" this.Name this.Version

[<Sealed>]
type ModuleNotFoundException (name: ModuleIdentifier, message) =
    inherit RuntimeException(ValueNone, message)

    member _.Name = name

let initialize program moduleImportLoader =
    /// A cache for the modules created by the import loader.
    let moduleImportResolver =
        let resolved = Dictionary<ModuleIdentifier, RuntimeModule> program.Imports.ImportedModules.Length
        let rec resolver import =
            match resolved.TryGetValue import with
            | true, existing -> existing
            | false, _ ->
                match moduleImportLoader import with
                | ValueSome import' ->
                    let r = RuntimeModule(import', resolver)
                    resolved.Add(import, r)
                    r
                | ValueNone ->
                    raise(ModuleNotFoundException(import, "Unable to find module " + string import))
        resolver

    RuntimeModule(program, moduleImportResolver)
