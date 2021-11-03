module rec UByte.Interpreter.Runtime

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

    member this.Clone() =
        let other = OffsetArray<'T> this.Length
        this.CopyTo other
        other

    static member Empty = OffsetArray<'T>(Array.Empty())

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
        source.RawData.CopyTo destination.RawData
        source.References.CopyTo destination.References

    let duplicateRuntimeValue (source: inref<RuntimeStruct>) =
        { RuntimeStruct.RawData = source.RawData.Clone(); References = source.References.Clone() }

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
module private RuntimeRegister = // TODO: Make it so registers only contain primtive type or object reference, no struct
    let primitiveType register =
        match register.RegisterType with
        | AnyType.ValueType(ValueType.Primitive prim) -> prim
        | _ -> invalidArg (nameof register) "Cannot retrieve numeric value from a register not containing a primitive type"

    let create rtype (dsize: int32) (rlen: int32) =
        { RuntimeRegister.RegisterValue =
            { RuntimeStruct.RawData = OffsetArray dsize
              RuntimeStruct.References = OffsetArray rlen }
          RegisterType = rtype }

    let clone source =
        { RuntimeRegister.RegisterType = source.RegisterType; RegisterValue = duplicateRuntimeValue &source.RegisterValue }

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
        localRegisterCount: uint32,
        returns: ImmutableArray<RuntimeRegister>,
        blocks: ImmutableArray<CodeBlock>,
        method: RuntimeMethod
    )
    =
    let mutable bindex, iindex = 0, 0
    let mutable previousBlockIndex = ValueNone
    let currentLocalLookup = Dictionary<LocalIndex, TemporaryIndex>(Checked.int32 localRegisterCount)
    let previousLocalLookup = Dictionary<LocalIndex, RuntimeRegister>(Checked.int32 localRegisterCount)
    do
        if not blocks.IsDefaultOrEmpty then
            // TODO: Avoid code duplication with JumpTo
            for struct(tindex, lindex) in blocks.[0].Locals do
                currentLocalLookup.Add(lindex, tindex)

    member _.ArgumentRegisters = args
    member _.InstructionIndex with get() = iindex and set i = iindex <- i
    member _.BlockIndex = bindex
    member _.PreviousBlockIndex = previousBlockIndex
    member val TemporaryRegisters = List<RuntimeRegister>()
    member _.ReturnRegisters = returns
    member _.Code = blocks
    member _.CurrentMethod = method
    member _.CurrentModule = method.Module
    member this.CurrentBlock = blocks.[this.BlockIndex]
    member _.Previous = prev

    member this.CurrentExceptionHandler = this.CurrentBlock.ExceptionHandler

    member this.RegisterAt(RegisterIndex.Index index) =
        let tcount = uint32 this.TemporaryRegisters.Count
        if index < tcount then
            this.TemporaryRegisters.[Checked.int32 index]
        else
            let index' = Checked.(-) index tcount
            let acount = uint32 this.ArgumentRegisters.Length
            if index' < acount then
                this.ArgumentRegisters.[Checked.int32 index']
            else
                let index' = LocalIndex.Index(index - tcount - acount)
                match currentLocalLookup.TryGetValue index' with
                | true, Index i -> this.TemporaryRegisters.[Checked.int32 i]
                | false, _ ->
                    match previousLocalLookup.TryGetValue index' with
                    | true, lregister -> lregister
                    | false, _ ->
                        sprintf "A register corresponding to the index 0x%0X could not be found" index
                        |> KeyNotFoundException
                        |> raise

    member this.JumpTo index =
        previousBlockIndex <- ValueSome bindex
        bindex <- index
        iindex <- -1
        for KeyValue(lindex, Index lregister) in currentLocalLookup do
            previousLocalLookup.[lindex] <- this.TemporaryRegisters.[Checked.int32 lregister]
        currentLocalLookup.Clear()
        this.TemporaryRegisters.Clear()
        for struct(tindex, lindex) in this.CurrentBlock.Locals do
            currentLocalLookup.Add(lindex, tindex)

    member _.AddExceptionRegister(index, register) = previousLocalLookup.Add(index, register)

    member this.StackTrace =
        let trace = System.Text.StringBuilder()
        let mutable current = ValueSome this
        while current.IsSome do
            let current' = current.Value
            Printf.bprintf trace "  at %O" current'.CurrentMethod
            if not current'.CurrentMethod.IsExternal then
                Printf.bprintf trace " block %i instruction 0x%04X" current'.BlockIndex current'.InstructionIndex
            current <- current'.Previous
            if current.IsSome then trace.Append Environment.NewLine |> ignore
        trace.ToString()

type InterpreterExecutionException =
    inherit Exception

    val private frame: RuntimeStackFrame voption

    new (frame, message, inner: Exception) = { inherit Exception(message, inner); frame = frame }

    new (frame, message) = InterpreterExecutionException(frame, message, null)

    member this.SourceFrame = this.frame

[<Sealed>]
type RuntimeException =
    inherit InterpreterExecutionException

    val ExceptionType: AnyType
    val ExceptionValue: RuntimeStruct

    new (frame, message, etype, evalue) =
        { inherit InterpreterExecutionException(ValueSome frame, message)
          ExceptionType = etype
          ExceptionValue = evalue }

type MethodInvocationResult = ImmutableArray<RuntimeRegister>

[<Sealed>]
type RuntimeArray =
    val Length: int32
    val Data: byte[]
    val References: RuntimeObject[]
    val ElementType: AnyType // TODO: Array element type only works with primitive types, cases that use type indices would be invalid if used outside of the intended module.

    new (rawDataSize, objectReferencesLength, length, etype) =
        if length < 0 then raise(ArgumentOutOfRangeException(nameof length, length, "The length of an array cannot be negative"))
        { Length = length
          Data = Array.zeroCreate<byte> (Checked.(*) rawDataSize length)
          References = Array.zeroCreate<RuntimeObject> (Checked.(*) objectReferencesLength length)
          ElementType = etype }

    new (stype: RuntimeTypeDefinition, length, etype) =
        let layout = stype.Layout
        RuntimeArray(layout.RawDataSize, layout.ObjectReferencesLength, length, etype)

    member private this.ElementLength arr = if this.Length > 0 then Array.length arr / this.Length else 0
    member private this.ElementReferences i = OffsetArray<RuntimeObject>(i * this.ElementLength this.References, this.References)

    member this.DataLength = this.ElementLength this.Data
    member this.ReferenceCount = this.ElementLength this.References

    member private this.CheckIndex i = if i >= this.Length then raise(IndexOutOfRangeException())

    member this.Item
        with get(index: int32): RuntimeStruct =
            this.CheckIndex index
            { RawData = OffsetArray<byte>(index * this.DataLength, this.Data)
              References = this.ElementReferences index }
        and set index (value: RuntimeStruct) =
            this.CheckIndex index
            value.RawData.AsSpan().CopyTo(Span(this.Data, index * this.DataLength, value.RawData.Length))
            let rdestination = Span(this.References, index * this.ReferenceCount, value.References.Length)
            value.References.AsSpan().CopyTo rdestination

[<RequireQualifiedAccess; NoComparison; ReferenceEquality>]
type RuntimeObject =
    | Null
    /// Represents a class instance or a boxed value type.
    | TypeInstance of otype: RuntimeTypeDefinition * fields: RuntimeStruct
    | Array of RuntimeArray
    | UnsafePointer of RuntimeStruct

type RuntimeStruct with
    member this.ReadRaw<'T when 'T : struct and 'T :> System.ValueType and 'T : (new: unit -> 'T)> (offset: int32): 'T =
        MemoryMarshal.Read<'T>(Span.op_Implicit(this.RawData.AsSpan()).Slice(offset))

    member this.WriteRaw<'T when 'T : struct and 'T :> System.ValueType and 'T : (new: unit -> 'T)>(offset, value: 'T) =
        let mutable value = value
        MemoryMarshal.Write<'T>(this.RawData.AsSpan().Slice(offset), &value)

    member this.ReadRef index = this.References.AsSpan().[index]

    member this.WriteRef(index, o) = this.References.AsSpan().[index] <- o

[<RequireQualifiedAccess>]
module Interpreter =
    open UByte.Format.Model.InstructionSet

    let private copyRegisterValues (source: ImmutableArray<RuntimeRegister>) (dest: ImmutableArray<RuntimeRegister>) =
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

        let not vtype register (destination: inref<_>) =
            unop (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~)
                (fun (Reinterpret.F32 i) ->  Reinterpret.(|U32|) (~~~i))
                (fun (Reinterpret.F64 i) -> Reinterpret.(|U64|) (~~~i))
                vtype register &destination

        let inline private rotop sh8 sh16 sh32 sh64 shnative vtype amount register (destination: inref<RuntimeStruct>) =
            let amount' = NumberValue.s32 amount
            match vtype with
            | PrimitiveType.Bool
            | PrimitiveType.S8
            | PrimitiveType.U8 ->
                destination.WriteRaw<uint8>(0, sh8 (NumberValue.u8 register) amount')
            | PrimitiveType.Char16
            | PrimitiveType.S16
            | PrimitiveType.U16 ->
                destination.WriteRaw<uint16>(0, sh16 (NumberValue.u16 register) amount')
            | PrimitiveType.Char32
            | PrimitiveType.S32
            | PrimitiveType.U32 ->
                destination.WriteRaw<uint32>(0, sh32 (NumberValue.u32 register) amount')
            | PrimitiveType.S64
            | PrimitiveType.U64 ->
                destination.WriteRaw<uint64>(0, sh64 (NumberValue.u64 register) amount')
            | PrimitiveType.SNative
            | PrimitiveType.UNative ->
                destination.WriteRaw<unativeint>(0, shnative (NumberValue.unative register) amount')
            | PrimitiveType.F32
            | PrimitiveType.F64 ->
                raise(NotImplementedException "Integer rotation not supported for floating point values, TODO: Reinterpret float as integer")
            | PrimitiveType.Unit ->
                noUnitType()

        let rotl vtype amount register (destination: inref<_>) =
            rotop (<<<) (<<<) (<<<) (<<<) (<<<) vtype amount register &destination

        let rotr vtype amount register (destination: inref<_>) =
            rotop (>>>) (>>>) (>>>) (>>>) (>>>) vtype amount register &destination

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

        let isLessThan xreg yreg = comparison (<) (<) (<) (<) (<) (<) (<) (<) (<) (<) (<) (<) (fun _ _ -> false) xreg yreg
        let isGreaterThan xreg yreg = comparison (>) (>) (>) (>) (>) (>) (>) (>) (>) (>) (>) (>) (fun _ _ -> false) xreg yreg
        let private refeq a b = Object.ReferenceEquals(a, b)
        let isEqual xreg yreg = comparison (=) (=) (=) (=) (=) (=) (=) (=) (=) (=) (=) (=) refeq xreg yreg
        let isLessOrEqual xreg yreg = comparison (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) refeq xreg yreg
        let isGreaterOrEqual xreg yreg = comparison (>=) (>=) (>=) (>=) (>=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) refeq xreg yreg

    /// Contains functions for storing values into registers.
    [<RequireQualifiedAccess>]
    module private Constant =
        let inline private number u8 s8 u16 s16 u32 s32 u64 s64 unative snative f32 f64 vtype value (destination: inref<RuntimeStruct>) =
            match vtype with
            | PrimitiveType.U8 | PrimitiveType.Bool -> destination.WriteRaw<uint8>(0, u8 value)
            | PrimitiveType.S8 -> destination.WriteRaw<int8>(0, s8 value)
            | PrimitiveType.U16 | PrimitiveType.Char16 -> destination.WriteRaw<uint16>(0, u16 value)
            | PrimitiveType.S16 -> destination.WriteRaw<int16>(0, s16 value)
            | PrimitiveType.U32 | PrimitiveType.Char32 -> destination.WriteRaw<uint32>(0, u32 value)
            | PrimitiveType.S32 -> destination.WriteRaw<int32>(0, s32 value)
            | PrimitiveType.U64 -> destination.WriteRaw<uint64>(0, u64 value)
            | PrimitiveType.S64 -> destination.WriteRaw<int64>(0, s64 value)
            | PrimitiveType.F32 -> destination.WriteRaw<single>(0, f32 value)
            | PrimitiveType.F64 -> destination.WriteRaw<double>(0, f64 value)
            | PrimitiveType.UNative -> destination.WriteRaw<unativeint>(0, unative value)
            | PrimitiveType.SNative -> destination.WriteRaw<nativeint>(0, snative value)
            | PrimitiveType.Unit -> ()

        let uinteger vtype (value: uint32) (destination: inref<_>) =
            number uint8 int8 uint16 int16 uint32 int32 uint64 int64 unativeint nativeint float32 float vtype value &destination

        let sinteger vtype (value: int32) (destination: inref<_>) =
            number uint8 int8 uint16 int16 uint32 int32 uint64 int64 unativeint nativeint float32 float vtype value &destination

        module Checked =
            open Microsoft.FSharp.Core.Operators.Checked

            let sinteger vtype (value: int32) (destination: inref<_>) =
                number uint8 int8 uint16 int16 uint32 int32 uint64 int64 unativeint nativeint float32 float vtype value &destination

        let private noBooleanFloat() = invalidOp "Conversion of boolean value to float is not supported"

        let boolean vtype value (destination: inref<RuntimeStruct>) =
            match vtype with
            | PrimitiveType.Bool | PrimitiveType.S8 | PrimitiveType.U8 ->
                destination.WriteRaw<bool>(0, value)
            | PrimitiveType.S16 | PrimitiveType.U16 | PrimitiveType.Char16 ->
                destination.WriteRaw<uint16>(0, if value then 1us else 0us)
            | PrimitiveType.S32 | PrimitiveType.U32 | PrimitiveType.Char32 ->
                destination.WriteRaw<uint32>(0, if value then 1u else 0u)
            | PrimitiveType.S64 | PrimitiveType.U64 ->
                destination.WriteRaw<uint64>(0, if value then 1UL else 0UL)
            | PrimitiveType.SNative | PrimitiveType.UNative ->
                destination.WriteRaw<unativeint>(0, if value then 1un else 0un)
            | PrimitiveType.F32 | PrimitiveType.F64 ->
                noBooleanFloat()
            | PrimitiveType.Unit -> ()

    let private (|ValidArithmeticFlags|) (flags: ArithmeticFlags) =
        if flags &&& (~~~ArithmeticFlags.ValidMask) <> ArithmeticFlags.None then
            failwithf "TODO: Bad arithmetic flags %A" flags
        flags

    let private (|ValidAllocationFlags|) (flags: AllocationFlags) =
        if flags &&& (~~~AllocationFlags.ValidMask) <> AllocationFlags.None then
            failwithf "TODO: Bad allocation flags %A" flags
        flags

    let private (|ValidMemoryAccessFlags|) mask (flags: MemoryAccessFlags) =
        if flags &&& (~~~mask) <> MemoryAccessFlags.None then
            failwithf "TODO: Bad memory access flags %A" flags
        flags

    [<Sealed>]
    type private RuntimeValueStack (dataBytesLength, objectReferenceCount) =
        let data = lazy Array.zeroCreate<byte> dataBytesLength

        let refs =
            lazy
                let allocated = Array.zeroCreate<RuntimeObject> objectReferenceCount
                Span(allocated).Fill(RuntimeObject.Null)
                allocated

        let mutable datai, refi = 0, 0

        let lengths = Stack<struct(int * int)>()

        member _.TryAlloc(bytes, references, value: outref<_>) =
            if datai + bytes < data.Value.Length && refi + references < refs.Value.Length then
                let dstart = datai
                let rstart = refi
                datai <- Checked.(+) datai bytes
                refi <- Checked.(+) refi references
                value <-
                    { RuntimeStruct.RawData = OffsetArray(dstart, data.Value)
                      References = OffsetArray(rstart, refs.Value) }
                true
            else
                false

        member _.PushAllocations() = lengths.Push((datai, refi))

        member _.PopAllocations() =
            let struct(prevDataIndex, prevReferenceIndex) = lengths.Pop()
            datai <- prevDataIndex
            refi <- prevReferenceIndex

    [<RequireQualifiedAccess>]
    module private MemoryOperations =
        let access address accessor =
            match address.RegisterValue.ReadRef 0 with
            | RuntimeObject.UnsafePointer memory -> accessor memory
            | _ -> invalidOp "Expected address register to contain a pointer"

    let [<Literal>] private MaxStackDataLength = 0xFFFFF

    let interpret arguments (entrypoint: RuntimeMethod) =
        let frame: RuntimeStackFrame voption ref = ref ValueNone
        let values = RuntimeValueStack(MaxStackDataLength, MaxStackDataLength / 8)
        let mutable runExternalCode: (RuntimeStackFrame voption ref -> unit) voption = ValueNone
        let mutable ex = ValueNone

        let invoke flags (arguments: ImmutableArray<_>) (method: RuntimeMethod) =
            if isFlagSet CallFlags.RequiresTailCallOptimization flags then
                raise(NotImplementedException "Tail call optimization is not yet supported")
            values.PushAllocations()
            method.SetupStackFrame(frame, &runExternalCode)
            let frame' = frame.Value.Value
            let arguments' = frame'.ArgumentRegisters
            if arguments.Length <> arguments'.Length then
                failwithf "TODO: Error for argument array lengths do not match, expected %i got %i" arguments'.Length arguments.Length
            if arguments.Length < arguments'.Length then
                invalidOp (sprintf "An instruction that calls a method must provide at least as many arguments as specified in the method signature, %i arguments were expected when only %i were provided" arguments'.Length arguments.Length)
            copyRegisterValues arguments arguments'
            frame'.ReturnRegisters

        let entryPointResults = invoke CallFlags.None arguments entrypoint

#if DEBUG
        let cont() =
#else
        let inline cont() =
#endif
            match frame.Value with
            | ValueSome frame' -> frame'.BlockIndex < frame'.Code.Length && frame'.InstructionIndex < frame'.CurrentBlock.Instructions.Length
            | ValueNone -> false

        while cont() do
            match ex with
            | ValueSome(e: exn) -> // TODO: Don't forget to call PopAllocations if moving to a previous frame
                let frame' = frame.Value.Value
                match frame'.CurrentExceptionHandler with
                | ValueSome({ CatchBlock = Index catch } as eh) ->
                    frame'.JumpTo(Checked.int32 catch)

                    match eh.ExceptionRegister with
                    | ValueSome eindex ->
                        let value =
                            match e with
                            | :? RuntimeException as rex ->
                                { RuntimeRegister.RegisterType = rex.ExceptionType
                                  RegisterValue = rex.ExceptionValue }
                            | _ -> raise(NotImplementedException "TODO: How to let user code handle internal exception?")

                        frame'.AddExceptionRegister(eindex, value)
                    | ValueNone -> ()

                    frame'.InstructionIndex <- 0
                | ValueNone ->
                    InterpreterExecutionException (
                        frame.Value,
                        "Unhandled runtime exception: " + Environment.NewLine + frame'.StackTrace,
                        e
                    )
                    |> raise
            | ValueNone -> ()

            let frame' = frame.Value.Value

#if DEBUG
            let (|Register|) rindex =
#else
            let inline (|Register|) rindex =
#endif
                frame'.RegisterAt rindex

            let (|LookupRegisterArray|) (indices: ImmutableArray<RegisterIndex>) =
                let mutable registers = Array.zeroCreate indices.Length // TODO: Cache register lookup array.
                for i = 0 to registers.Length - 1 do registers.[i] <- frame'.RegisterAt indices.[i]
                Unsafe.As<RuntimeRegister[], ImmutableArray<RuntimeRegister>> &registers

            let inline (|Method|) mindex: RuntimeMethod = frame'.CurrentModule.InitializeMethod mindex
            let inline (|Field|) findex: RuntimeField = frame'.CurrentModule.InitializeField findex
            let inline (|DeclaringType|) m: RuntimeTypeDefinition = (^Member : (member DeclaringType : RuntimeTypeDefinition) m)
            let inline (|TypeSignature|) tindex: AnyType = frame'.CurrentModule.TypeSignatureAt tindex
            let inline (|TypeLayout|) (t: RuntimeTypeDefinition) = t.Layout
            let inline (|Data|) dindex: ImmutableArray<_> = frame'.CurrentModule.DataAt dindex
            let inline (|BranchTarget|) (target: BlockOffset) = Checked.(+) frame'.BlockIndex target

#if DEBUG
            let branchToTarget (BranchTarget target) =
#else
            let inline branchToTarget (BranchTarget target) =
#endif
                frame'.JumpTo target

#if DEBUG
            let inline fieldAccessInstruction field object access =
#else
            let fieldAccessInstruction field object access =
#endif
                if object.RegisterValue.References.Length = 0 then
                    raise(NotImplementedException "TODO: How to handle field access for struct?")
                match object.RegisterValue.ReadRef 0 with
                | RuntimeObject.UnsafePointer _ ->
                    raise(NotImplementedException "TODO: How to handle field access using unsafe pointers?")
                | RuntimeObject.TypeInstance(TypeLayout layout, data) ->
                    let struct(dindex, rindex) = layout.FieldIndices.[field]
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
                    invalidOp("Cannot access array element with an object reference of type " + otype.ToString())
                | RuntimeObject.UnsafePointer _ ->
                    invalidOp "Cannot access array element with an unsafe pointer"
                | RuntimeObject.Null ->
                    raise(NullReferenceException "Cannot access an array element with a null array reference")

            let inline createPrimitiveRegister rtype =
                let register = RuntimeRegister.Raw(primitiveTypeSize rtype, ValueType.Primitive rtype)
                frame'.TemporaryRegisters.Add register
                register

            let inline createReferenceRegister rtype =
                let register = RuntimeRegister.Object rtype
                frame'.TemporaryRegisters.Add register
                register

            try
                let instr = frame'.CurrentBlock.Instructions.[frame'.InstructionIndex]

#if DEBUG && TRACE_EXECUTION
                printfn "%O -" frame'.CurrentMethod
                printfn "\t%A at block %i instruction %i" instr frame'.BlockIndex frame'.InstructionIndex 
#endif
                match instr with
                | Phi values -> // TODO: In format, reverse order of indices so BlockOffset is first to allow usage as key in dictionary.
                    match frame'.PreviousBlockIndex with
                    | ValueSome prev ->
                        // TODO: Use for loop to avoid extra allocations, maybe even require sorting in the binary format.
                        let struct(valuei, _) = Seq.find (fun struct(_, BranchTarget blocki) -> blocki = prev) values
                        frame'.TemporaryRegisters.Add(RuntimeRegister.clone(frame'.RegisterAt valuei))
                    | ValueNone ->
                        invalidOp "Usage of phi instruction is prohibited in the first block of a method"
                | Select(Register condition, Register vtrue, Register vfalse) ->
                    if Compare.isTrueValue condition then vtrue else vfalse
                    |> RuntimeRegister.clone
                    |> frame'.TemporaryRegisters.Add
                // TODO: Create common helper function for arithmetic operations, and don't use inref<_> since RuntimeRegister can be used directly
                | Add(ValidArithmeticFlags flags, vtype, Register x, Register y) ->
                    let destination = createPrimitiveRegister vtype
                    if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                    then Arithmetic.Checked.add vtype x y &destination.RegisterValue
                    else Arithmetic.add vtype x y &destination.RegisterValue
                | Sub(ValidArithmeticFlags flags, vtype, Register x, Register y) ->
                    let destination = createPrimitiveRegister vtype
                    if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                    then Arithmetic.Checked.sub vtype x y &destination.RegisterValue
                    else Arithmetic.sub vtype x y &destination.RegisterValue
                | Mul(ValidArithmeticFlags flags, vtype, Register x, Register y) ->
                    let destination = createPrimitiveRegister vtype
                    if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                    then Arithmetic.Checked.mul vtype x y &destination.RegisterValue
                    else Arithmetic.mul vtype x y &destination.RegisterValue
                | Div(ValidArithmeticFlags flags, vtype, Register x, Register y) ->
                    let destination = createPrimitiveRegister vtype
                    if isFlagSet ArithmeticFlags.ThrowOnDivideByZero flags
                    then Arithmetic.Checked.div vtype x y &destination.RegisterValue
                    else Arithmetic.div vtype x y &destination.RegisterValue
                | Rem(ValidArithmeticFlags flags, vtype, Register x, Register y) ->
                    let destination = createPrimitiveRegister vtype
                    if isFlagSet ArithmeticFlags.ThrowOnDivideByZero flags
                    then Arithmetic.Checked.rem vtype x y &destination.RegisterValue
                    else Arithmetic.rem vtype x y &destination.RegisterValue
                | Incr(ValidArithmeticFlags flags, vtype, Register register) ->
                    let destination = createPrimitiveRegister vtype
                    if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                    then Arithmetic.Checked.incr vtype register &destination.RegisterValue
                    else Arithmetic.incr vtype register &destination.RegisterValue
                | Decr(ValidArithmeticFlags flags, vtype, Register register) ->
                    let destination = createPrimitiveRegister vtype
                    if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                    then Arithmetic.Checked.decr vtype register &destination.RegisterValue
                    else Arithmetic.decr vtype register &destination.RegisterValue
                | And(vtype, Register x, Register y) ->
                    let destination = createPrimitiveRegister vtype
                    Arithmetic.``and`` vtype x y &destination.RegisterValue
                | Or(vtype, Register x, Register y) ->
                    let destination = createPrimitiveRegister vtype
                    Arithmetic.``or`` vtype x y &destination.RegisterValue
                | Xor(vtype, Register x, Register y) ->
                    let destination = createPrimitiveRegister vtype
                    Arithmetic.xor vtype x y &destination.RegisterValue
                | Not(vtype, Register register) ->
                    let destination = createPrimitiveRegister vtype
                    Arithmetic.not vtype register &destination.RegisterValue
                // TODO: Should exception be thrown if Constant integer overflows?
                | Const_u(vtype, value) ->
                    let destination = createPrimitiveRegister vtype
                    Constant.uinteger vtype value &destination.RegisterValue
                | Const_s(vtype, value) ->
                    let destination = createPrimitiveRegister vtype
                    Constant.sinteger vtype value &destination.RegisterValue
                | Const_true vtype ->
                    let destination = createPrimitiveRegister vtype
                    Constant.boolean vtype true &destination.RegisterValue
                | Const_false vtype | Const_zero vtype ->
                    let destination = createPrimitiveRegister vtype
                    Constant.boolean vtype false &destination.RegisterValue
                | Obj_null -> (createReferenceRegister ReferenceType.Any).RegisterValue.WriteRef(0, RuntimeObject.Null)
                | Const_f32 _
                | Const_f64 _ -> failwith "TODO: Storing of constant floating point integers is not yet supported"
                | Call(flags, Method method, LookupRegisterArray arguments) ->
                    frame'.TemporaryRegisters.AddRange(invoke flags arguments method)
                | Call_virt(flags, Method method, Register this, LookupRegisterArray arguments) ->
                    if not(isFlagSet CallFlags.ThrowOnNullThis flags) then
                        invalidOp "Calling virtual method without checking for null object reference is not supported"

                    match this.RegisterValue.ReadRef 0 with
                    | RuntimeObject.TypeInstance(otype, _) -> 
                        frame'.TemporaryRegisters.AddRange(invoke flags (arguments.Insert(0, this)) otype.VTable.[method])
                    | RuntimeObject.Null ->
                        "Cannot call virtual method with null object reference since the type containing the method to call " +
                        "cannot be deduced"
                        |> invalidOp
                    | RuntimeObject.Array _ ->
                        invalidOp "Cannot call virtual method with an array object reference"
                    | RuntimeObject.UnsafePointer _ ->
                        "Cannot call virual method with unsafe pointer since the type containing the method to call cannot be " +
                        "deduced, use a safe pointer to ensure type information is included"
                        |> invalidOp
                | Obj_new(Method constructor, LookupRegisterArray arguments) ->
                    let o = RuntimeObject.TypeInstance(constructor.DeclaringType, constructor.DeclaringType.InitializeObjectFields())
                    let destination = createReferenceRegister(ReferenceType.Defined constructor.DeclaringType.Index) // TODO: Cache the ReferenceType in RuntimeTypeDefinition.
                    destination.RegisterValue.WriteRef(0, o)
                    invoke CallFlags.None (arguments.Insert(0, destination)) constructor |> ignore
                | Obj_fd_ld(Field field, Register object) ->
                    field.CheckMutate frame'
                    let destination = { RuntimeRegister.RegisterType = field.FieldType; RegisterValue = field.AllocateValue() }
                    frame'.TemporaryRegisters.Add destination
                    fieldAccessInstruction field object <| fun value -> copyRuntimeValue &value &destination.RegisterValue
                | Obj_fd_st(Field field, Register object, Register source) ->
                    fieldAccessInstruction field object <| fun value -> copyRuntimeValue &source.RegisterValue &value
                | Obj_fd_addr(ValidMemoryAccessFlags MemoryAccessFlags.ElementAccessValidMask flags, Field field, Register object) ->
                    // TODO: If object register does not contain the field, throw exception if exn flag is set.
                    fieldAccessInstruction field object <| fun value ->
                        { RuntimeRegister.RegisterType = AnyType.ReferenceType ReferenceType.Any
                          RegisterValue =
                            { RuntimeStruct.RawData = OffsetArray.Empty
                              References = OffsetArray(Array.singleton(RuntimeObject.UnsafePointer value)) } }
                        |> frame'.TemporaryRegisters.Add
                | Obj_arr_new(TypeSignature etype, Register length) ->
                    let struct(dsize, rlen) = frame'.CurrentMethod.Module.CalculateTypeSize etype
                    let array = RuntimeArray(dsize, rlen, NumberValue.s32 length, etype)
                    createReferenceRegister((*ReferenceType.Vector etype*) ReferenceType.Any).RegisterValue.WriteRef(0, RuntimeObject.Array array) // TODO: Get/check array element type.
                | Obj_arr_const(TypeSignature etype, Data data) ->
                    let struct(dsize, rlen) = frame'.CurrentMethod.Module.CalculateTypeSize etype
                    if rlen > 0 then invalidOp("Cannot create constant array containing elements of type " + etype.ToString())
                    let array = RuntimeArray(dsize, rlen, data.Length / dsize, etype)
                    data.AsSpan().Slice(0, array.Data.Length).CopyTo(Span array.Data)
                    createReferenceRegister((*ReferenceType.Vector etype*) ReferenceType.Any).RegisterValue.WriteRef(0, RuntimeObject.Array array) // TODO: Get/check array element type.
                | Obj_arr_len(ValidArithmeticFlags flags, ltype, Register array) ->
                    let destination = createPrimitiveRegister ltype
                    match array.RegisterValue.ReadRef 0 with
                    | RuntimeObject.Array array' ->
                        if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                        then Constant.Checked.sinteger ltype array'.Length &destination.RegisterValue
                        else Constant.sinteger ltype array'.Length &destination.RegisterValue
                    | RuntimeObject.Null ->
                        raise(NullReferenceException "Cannot access array length with a null array reference")
                    | RuntimeObject.TypeInstance(otype, _) ->
                        invalidOp("Cannot access array length with an object reference of type " + otype.ToString())
                    | RuntimeObject.UnsafePointer _ ->
                        invalidOp "Cannot determine array length using an unsafe pointer"
                | Obj_arr_get(Register array, Register index) ->
                    arrayAccessInstruction array index <| fun array i ->
                        let value = array.[i]
                        let destination =
                            { RuntimeRegister.RegisterType = array.ElementType
                              RegisterValue =
                                { RuntimeStruct.RawData = OffsetArray array.DataLength
                                  References = OffsetArray array.ReferenceCount } }
                        frame'.TemporaryRegisters.Add destination
                        copyRuntimeValue &value &destination.RegisterValue
                | Obj_arr_set(Register array, Register index, Register source) ->
                    arrayAccessInstruction array index <| fun array i -> array.[i] <- source.RegisterValue
                | Obj_arr_addr(ValidMemoryAccessFlags MemoryAccessFlags.ElementAccessValidMask flags, Register array, Register index) ->
                    arrayAccessInstruction array index <| fun array i ->
                        // TODO: What type to use for pointer to array element? array.ElementType may be a reference type but ValueType.UnsafePointer only applies to ValueTypes.
                        // TODO: If array index is out of bounds and exn flag is set, throw exception.
                        { RuntimeRegister.RegisterType = AnyType.ReferenceType ReferenceType.Any
                          RegisterValue =
                            { RuntimeStruct.RawData = OffsetArray.Empty
                              References = OffsetArray(Array.singleton(RuntimeObject.UnsafePointer array.[i])) } }
                        |> frame'.TemporaryRegisters.Add
                | Ret(LookupRegisterArray results) ->
                    if results.Length < frame'.ReturnRegisters.Length then
                        invalidOp(sprintf "Expected to return %i values but only returned %i values" frame'.ReturnRegisters.Length results.Length)
                    copyRegisterValues results frame'.ReturnRegisters
                    frame.Value <- frame'.Previous
                    values.PopAllocations()
                | Br target -> branchToTarget target
                | Br_eq(Register x, Register y, ttrue, tfalse)
                | Br_ne(Register x, Register y, tfalse, ttrue) ->
                    branchToTarget (if Compare.isEqual x y then ttrue else tfalse)
                | Br_lt(Register x, Register y, tfalse, ttrue) ->
                    branchToTarget (if Compare.isLessThan x y then ttrue else tfalse)
                | Br_gt(Register x, Register y, tfalse, ttrue) ->
                    branchToTarget (if Compare.isGreaterThan x y then ttrue else tfalse)
                | Br_le(Register x, Register y, tfalse, ttrue) ->
                    branchToTarget (if Compare.isLessOrEqual x y then ttrue else tfalse)
                | Br_ge(Register x, Register y, tfalse, ttrue) ->
                    branchToTarget (if Compare.isGreaterOrEqual x y then ttrue else tfalse)
                | Br_true(Register condition, ttrue, tfalse) ->
                    branchToTarget (if Compare.isTrueValue condition then ttrue else tfalse)
                | Obj_throw(Register e) ->
                    ex <- ValueSome(RuntimeException(frame', "Runtime exception has been thrown", e.RegisterType, e.RegisterValue) :> exn)
                | Mem_st(ValidMemoryAccessFlags MemoryAccessFlags.RawAccessValidMask flags, Register value, TypeSignature t, Register address) -> // TODO: Throw exception on invalid access (store)
                    MemoryOperations.access address <| fun ptr -> copyRuntimeValue &value.RegisterValue &ptr
                | Mem_ld(ValidMemoryAccessFlags MemoryAccessFlags.RawAccessValidMask flags, TypeSignature t, Register address) -> // TODO: Throw exception on invalid access (load)
                    let struct(dlen, rlen) = frame'.CurrentModule.CalculateTypeSize t
                    let destination =
                        { RuntimeRegister.RegisterValue =
                            { RuntimeStruct.RawData = OffsetArray dlen
                              References = OffsetArray rlen }
                          RegisterType = t }
                    MemoryOperations.access address <| fun ptr ->
                        ptr.RawData.AsSpan().Slice(0, dlen).CopyTo(destination.RegisterValue.RawData.AsSpan())
                        ptr.References.AsSpan().Slice(0, rlen).CopyTo(destination.RegisterValue.References.AsSpan())
                | Mem_cpy(ValidMemoryAccessFlags MemoryAccessFlags.RawAccessValidMask flags, Register count, TypeSignature t, Register source, Register destination) ->
                    let struct(dlen, rlen) = frame'.CurrentModule.CalculateTypeSize t
                    let count' = NumberValue.s32 count
                    MemoryOperations.access source <| fun src -> MemoryOperations.access destination <| fun dest ->
                        src.RawData.AsSpan().Slice(0, dlen * count').CopyTo(dest.RawData.AsSpan())
                        src.References.AsSpan().Slice(0, rlen * count').CopyTo(dest.References.AsSpan())
                | Alloca(ValidAllocationFlags flags, Register count, TypeSignature t) -> // TODO: Have alloca-like instructions return a boolean indicating success instead.
                    let count' = NumberValue.s32 count
                    let struct(dlen, rlen) = frame'.CurrentModule.CalculateTypeSize t
                    let success, allocated = values.TryAlloc(Checked.(*) dlen count', Checked.(*) rlen count')
                    if success then
                        frame'.TemporaryRegisters.Add
                            { RuntimeRegister.RegisterValue =
                                { RuntimeStruct.RawData = OffsetArray.Empty
                                  References = OffsetArray(Array.singleton(RuntimeObject.UnsafePointer allocated)) }
                              RegisterType = t }
                    elif isFlagSet AllocationFlags.ThrowOnFailure flags then
                        invalidOp "Failed to allocate on stack, maximum capacity exceeded"
                    else
                        raise(NotImplementedException "How to handle failing stack allocations without exceptions? Return an empty zero array?")
                | Alloca_obj _ ->
                    raise(NotImplementedException "TODO: Allocate memory then call constructor")
                | Mem_init(ValidMemoryAccessFlags MemoryAccessFlags.RawAccessValidMask flags, Register count, TypeSignature t, Register address, Register value) ->
                    raise(NotImplementedException "TODO: Is type needed for mem.init? Could just use type in value register.")
                | Mem_init_const(ValidMemoryAccessFlags MemoryAccessFlags.RawAccessValidMask flags, TypeSignature t, Register address, Data data) -> // TODO: Is type not needed for mem.init.const?
                    MemoryOperations.access address <| fun ptr ->
                        // TODO: Check flags to see if exn should be thrown on invalid access
                        data.AsSpan().CopyTo(ptr.RawData.AsSpan())
                | Nop -> ()
                | Rotl(vtype, Register amount, Register value) ->
                    let destination = createPrimitiveRegister vtype
                    Arithmetic.rotl vtype amount value &destination.RegisterValue
                | Rotr(vtype, Register amount, Register value) ->
                    let destination = createPrimitiveRegister vtype
                    Arithmetic.rotr vtype amount value &destination.RegisterValue

                match runExternalCode with
                | ValueNone -> ()
                | ValueSome run ->
                    run frame
                    runExternalCode <- ValueNone // TODO: Avoid code duplication with ret.
                    values.PopAllocations()

                // Incrementing here means index points to instruction that caused the exception in the stack frame.
                frame'.InstructionIndex <- Checked.(+) frame'.InstructionIndex 1
            with
            | e -> ex <- ValueSome e

        match ex with
        | ValueSome e -> raise e
        | ValueNone -> ()

        match frame.contents with
        | ValueNone -> entryPointResults
        | ValueSome frame' ->
            invalidOp(sprintf "Reached unexpected end of code in block %i" frame'.BlockIndex)

[<RequireQualifiedAccess>]
module ExternalCode =
    [<Literal>]
    let private InternalCall = "runmdl"

    let private lookup = Dictionary<struct(string * string), RuntimeStackFrame -> unit>()

    let private println (frame: RuntimeStackFrame) =
        match frame.ArgumentRegisters.[0].RegisterValue.ReadRef 0 with
        | RuntimeObject.Null -> ()
        | RuntimeObject.Array chars ->
            for i = 0 to chars.Length - 1 do
                let c = chars.[i].ReadRaw<System.Text.Rune> 0
                stdout.Write(c.ToString())
        | RuntimeObject.UnsafePointer data ->
            for i = 0 to (data.RawData.Length / 4) - 1 do // TODO: Figure out why it starts with null byte, and end at null byte.
                let c = data.ReadRaw<System.Text.Rune> i
                stdout.Write(c.ToString())
        | _ ->
            failwith "TODO: How to print some other thing"

        stdout.WriteLine()

    do lookup.[(InternalCall, "testhelperprintln")] <- println
    do lookup.[(InternalCall, "break")] <- fun _ -> System.Diagnostics.Debugger.Launch() |> ignore

    let call library name =
        match lookup.TryGetValue(struct(library, name)) with
        | true, call' -> call'
        | false, _ -> fun _ -> failwithf "TODO: Handle external calls to %s in %s" library name

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

    member private _.CreateRuntimeRegister(tindex: TypeSignatureIndex): RuntimeRegister =
        let t = rmodule.TypeSignatureAt tindex
        let struct(dsize, rlen) = rmodule.CalculateTypeSize t
        RuntimeRegister.create t dsize rlen

    member this.CreateArgumentRegisters() =
        let { MethodSignature.ParameterTypes = atypes } = rmodule.MethodSignatureAt method.Signature
        let mutable registers = Array.zeroCreate atypes.Length
        for i = 0 to registers.Length - 1 do registers.[i] <- this.CreateRuntimeRegister atypes.[i]
        Unsafe.As<RuntimeRegister[], ImmutableArray<RuntimeRegister>> &registers

    member this.CreateReturnRegisters() =
        let { MethodSignature.ReturnTypes = rtypes } = this.Signature
        let mutable returns = Array.zeroCreate rtypes.Length
        for i = 0 to returns.Length - 1 do returns.[i] <- this.CreateRuntimeRegister rtypes.[i]
        Unsafe.As<RuntimeRegister[], ImmutableArray<RuntimeRegister>> &returns

    member this.SetupStackFrame(frame: RuntimeStackFrame voption ref, runExternalCode: outref<_ voption>) =
        let returns = this.CreateReturnRegisters()
        match body with
        | MethodBody.Defined codei ->
            let code = rmodule.CodeAt codei
            let args = this.CreateArgumentRegisters()
            frame.contents <- ValueSome(RuntimeStackFrame(frame.Value, args, code.LocalCount, returns, code.Blocks, this))
        | MethodBody.Abstract ->
            if not this.IsVirtual then failwith "TODO: Error for abstract method must be virtual"
            invalidOp(sprintf "Cannot directly call %O, use the call.virt instruction and related instructions instead" this)
        | MethodBody.External(library, efunction) ->
            let library' = this.Module.IdentifierAt library
            let efunction' = this.Module.IdentifierAt efunction
            let args = this.CreateArgumentRegisters()
            let frame' = RuntimeStackFrame(frame.Value, args, 0u, returns, ImmutableArray.Empty, this)
            frame.contents <- ValueSome frame'
            runExternalCode <- ValueSome <| fun frame'' ->
                ExternalCode.call library' efunction' frame''.Value.Value
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

    member this.AllocateValue(): RuntimeStruct =
        { RuntimeStruct.RawData = OffsetArray this.DataSize; References = OffsetArray this.ReferencesLength }

    /// If the field is not marked as mutable, prevents modification of the field value outside of a constructor or type initializer.
    member this.CheckMutate(frame: RuntimeStackFrame) =
        if not this.IsMutable && not frame.CurrentMethod.IsConstructor then
            invalidOp "Attempted to modify read-only field outside of constructor or type initializer"

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type RuntimeTypeLayout =
    { Fields: ImmutableArray<RuntimeField>
      FieldIndices: IReadOnlyDictionary<RuntimeField, struct(int32 * int32)>
      RawDataSize: int32
      ObjectReferencesLength: int32 }

[<Sealed>]
type RecursiveInheritanceException (message, t: RuntimeTypeDefinition) =
    inherit Exception(message)
    member _.Type = t

[<NoComparison; NoEquality>]
type InheritedTypeLayout =
    { InheritedFieldCount: int32
      InheritedDataSize: int32
      InheritedReferencesLength: int32 }

[<Sealed>]
type RuntimeTypeDefinition (rm: RuntimeModule, index: TypeDefinitionIndex, t: TypeDefinition) as rt =
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
    member _.Index: TypeDefinitionIndex = index
    member _.InheritedTypes = fst findInheritedTypes.Value
    member _.Layout: RuntimeTypeLayout = layout.Value
    member _.VTable: IReadOnlyDictionary<RuntimeMethod, RuntimeMethod> = vtable.Value

    member val Name = rm.IdentifierAt t.TypeName
    member val Namespace = rm.NamespaceAt t.TypeNamespace

    member _.FindMethod name = // TODO: Figure out if methods defined in inherited class(es), note that this is used to resolve method references.
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
    inherit Exception(message)

    member _.Module = m

[<Sealed>]
type TypeNotFoundException (m: RuntimeModule, typeNamespace, typeName, message: string) =
    inherit Exception(message)

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
            (fun i i' -> RuntimeTypeDefinition(rm, i', definitions.[i]))
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

    member _.CalculateTypeSize(t: AnyType): struct(int32 * int32) =
        match t with
        | ValueType vt ->
            match vt with
            | ValueType.Primitive prim -> struct(primitiveTypeSize prim, 0)
            | ValueType.UnsafePointer _ -> struct(0, 1)
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
                            let inline characterArrayArguments ctype convert =
                                let argty =
                                    ValueType.Primitive ctype
                                    |> ReferenceOrValueType.Value
                                    |> ReferenceType.Vector

                                let argv' = RuntimeArray(0, 1, argv.Length, AnyType.ReferenceType argty)

                                for i = 0 to argv.Length - 1 do
                                    let arg = RuntimeStruct.Object()
                                    arg.WriteRef(0, RuntimeObject.Array(convert argv.[i]))
                                    argv'.[i] <- arg

                                let argv'' = RuntimeRegister.Object argt
                                argv''.RegisterValue.WriteRef(0, RuntimeObject.Array argv')
                                ImmutableArray.Create argv''

                            match tchar with // TODO: Create a Char8 type for UTF-8 strings
                            | ReferenceOrValueType.Value(ValueType.Primitive PrimitiveType.Char16 as c16) ->
                                characterArrayArguments PrimitiveType.Char16 <| fun arg ->
                                    let arg' = RuntimeArray(2, 0, arg.Length, AnyType.ValueType c16)
                                    for i = 0 to arg.Length - 1 do
                                        let c = RuntimeStruct.Raw 2
                                        c.WriteRaw(0, uint16 arg.[i])
                                        arg'.[i] <- c
                                    arg'
                            | ReferenceOrValueType.Value(ValueType.Primitive PrimitiveType.Char32 as c32) ->
                                let buffer = List()
                                characterArrayArguments PrimitiveType.Char32 <| fun arg ->
                                    buffer.Clear()
                                    for cu in arg.EnumerateRunes() do buffer.Add(uint32 cu.Value)
                                    let arg' = RuntimeArray(4, 0, buffer.Count, AnyType.ValueType c32)
                                    for i = 0 to buffer.Count - 1 do
                                        let c = RuntimeStruct.Raw 4
                                        c.WriteRaw(0, buffer.[i])
                                        arg'.[i] <- c
                                    arg'
                            | bad -> failwithf "TODO: Invalid character type %A" bad
                        | bad -> failwithf "TODO: Invalid string type %A" bad
                    | bad -> failwithf "TODO: Error for invalid entrypoint argument type %A" bad
                | _ -> failwith "TODO: Error for invalid number of arguments for entrypoint"

            let results = Interpreter.interpret arguments main
            let ecode =
                if not results.IsDefaultOrEmpty
                then results.[0].RegisterValue.ReadRaw<int32> 0
                else 0
#if DEBUG && TRACE_EXECUTION
            printfn "Exited with code %i (0x%08X)" ecode ecode
#endif
            ecode
        | ValueNone -> raise(MissingEntryPointException(this, "The entry point method of the module is not defined"))

    override this.ToString() = sprintf "(%s, v%O)" this.Name this.Version

[<Sealed>]
type ModuleNotFoundException (name: ModuleIdentifier, message) =
    inherit Exception(message)

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
