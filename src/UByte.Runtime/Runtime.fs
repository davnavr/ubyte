module rec UByte.Interpreter.Runtime

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

open UByte.Format.Model

let inline isFlagSet flag value = value &&& flag = flag

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type OffsetArray<'T> =
    val Start: int32
    val Items: 'T[]

    new (start, items) = { Start = start; Items = items }
    new (items) = OffsetArray(0, items)
    new (length) = OffsetArray(Array.zeroCreate<'T> length)

    member this.Length = this.Items.Length - this.Start

    member this.ToSpan() = Span<'T>(this.Items).Slice this.Start

    member this.CopyTo(destination: OffsetArray<'T>) = this.ToSpan().CopyTo(destination.ToSpan())

//type RuntimeRegister = RuntimeRegister of RuntimeStruct

[<RequireQualifiedAccess>]
type RuntimeRegister =
    | S8 of int8 ref
    | U8 of uint8 ref
    | S16 of int16 ref
    | U16 of uint16 ref
    | S32 of int32 ref
    | U32 of uint32 ref
    | S64 of int64 ref
    | U64 of uint64 ref
    | SNative of nativeint ref
    | UNative of unativeint ref
    | F32 of single ref
    | F64 of double ref
    | Struct of RuntimeStruct
    | Object of RuntimeObject ref
    //| SafePointer of _ -> _

[<RequireQualifiedAccess>]
module Cast =
    let inline (|U8|) value = uint8 value
    let inline (|U16|) value = uint16 value
    let inline (|U32|) value = uint32 value
    let inline (|U64|) value = uint64 value
    let inline (|UNative|) value = unativeint value
    let inline (|S8|) value = int8 value
    let inline (|S16|) value = int16 value
    let inline (|S32|) value = int32 value
    let inline (|S64|) value = int64 value
    let inline (|SNative|) value = nativeint value
    let inline (|F32|) value = single value
    let inline (|F64|) value = double value

[<RequireQualifiedAccess>]
module Reinterpret =
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

[<RequireQualifiedAccess>]
module private Const =
    let private expectedNumericRegister() =
        failwith "TODO: Error for cannot store integer into a register containing an object reference"

    let inline private integer value destination =
        match destination with
        | RuntimeRegister.S8 dest' -> dest'.contents <- int8 value
        | RuntimeRegister.U8 dest' -> dest'.contents <- uint8 value
        | RuntimeRegister.S16 dest' -> dest'.contents <- int16 value
        | RuntimeRegister.U16 dest' -> dest'.contents <- uint16 value
        | RuntimeRegister.S32 dest' -> dest'.contents <- int32 value
        | RuntimeRegister.U32 dest' -> dest'.contents <- uint32 value
        | RuntimeRegister.S64 dest' -> dest'.contents <- int64 value
        | RuntimeRegister.U64 dest' -> dest'.contents <- uint64 value
        | RuntimeRegister.SNative dest' -> dest'.contents <- nativeint value
        | RuntimeRegister.UNative dest' -> dest'.contents <- unativeint value
        | RuntimeRegister.F32 dest' -> dest'.contents <- Reinterpret.(|U32|) (uint32 value)
        | RuntimeRegister.F64 dest' -> dest'.contents <- Reinterpret.(|U64|) (uint64 value)
        | RuntimeRegister.Object _ | RuntimeRegister.Struct _ -> expectedNumericRegister()
        // TODO: Figure out how to handle coyping when floating-point numbers are involved.

    let inline private real value destination =
        match destination with
        | RuntimeRegister.F32 dest' -> dest'.contents <- single value
        | RuntimeRegister.F64 dest' -> dest'.contents <- double value
        | RuntimeRegister.Object _ | RuntimeRegister.Struct _ -> expectedNumericRegister()

    let i8 (value: uint8) destination = integer value destination
    let i16 (value: uint16) destination = integer value destination
    let i32 (value: uint32) destination = integer value destination
    let i64 (value: uint64) destination = integer value destination
    let inative (value: unativeint) destination = integer value destination
    let inline ``true`` destination = i8 1uy destination
    let inline ``false`` destination = i8 0uy destination
    let f32 (value: single) destination = real value destination
    let f64 (value: double) destination = real value destination

    let obj value destination =
        match destination with
        | RuntimeRegister.Object destination' -> destination'.contents <- value
        | _ -> failwith "TODO: Error for cannot store object into a register containing a value type"

    let value value destination =
        match destination with
        | RuntimeRegister.Struct destination' ->
            value.RawData.CopyTo destination'.RawData
            value.References.CopyTo destination'.References
        | _ -> failwith "TODO: Cannot store struct into a register containing a numeric type"

type RuntimeRegister with
    member source.CopyValueTo destination =
        match source with
        | S8 { contents = Cast.U8 value } | U8 { contents = value } -> Const.i8 value destination
        | S16 { contents = Cast.U16 value } | U16 { contents = value } -> Const.i16 value destination
        | S32 { contents = Cast.U32 value } | U32 { contents = value } -> Const.i32 value destination
        | S64 { contents = Cast.U64 value } | U64 { contents = value } -> Const.i64 value destination
        | SNative { contents = Cast.UNative value } | UNative { contents = value } -> Const.inative value destination
        | F32 { contents = value } -> Const.f32 value destination
        | F64 { contents = value } -> Const.f64 value destination
        | Object { contents = value } -> Const.obj value destination
        | Struct value -> Const.value value destination

[<Sealed>]
type RuntimeStackFrame
    (
        prev: RuntimeStackFrame voption,
        args: ImmutableArray<RuntimeRegister>,
        locals: ImmutableArray<RuntimeRegister>,
        returns: ImmutableArray<RuntimeRegister>,
        instructions: ImmutableArray<InstructionSet.Instruction>,
        method: RuntimeMethod // TODO: Should be Choice<RuntimeMethod, RuntimeTypeInitializer>.
    )
    =
    let mutable iindex = 0

    member _.ArgumentRegisters = args
    member _.InstructionIndex with get() = iindex and set i = iindex <- i
    member _.LocalRegisters = locals
    member _.ReturnRegisters = returns
    member _.Instructions = instructions
    member _.CurrentMethod = method
    member _.Previous = prev

    member this.StackTrace =
        let trace = System.Text.StringBuilder()
        let mutable current = ValueSome this
        while current.IsSome do
            let current' = current.Value
            Printf.bprintf trace "  at %O" current'.CurrentMethod
            if not current'.Instructions.IsDefaultOrEmpty then
                Printf.bprintf trace " offset 0x%04X" current'.InstructionIndex
            current <- current'.Previous
            if current.IsSome then trace.Append Environment.NewLine |> ignore
        trace.ToString()

    member this.RegisterAt (Index i: RegisterIndex) =
        let i' = Checked.int32 i
        if i' >= args.Length then this.LocalRegisters.[i' - args.Length] else args.[i']

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
type StructVector (stype: RuntimeTypeDefinition, length) =
    do if length < 0 then raise(ArgumentOutOfRangeException(nameof length, length, "The length of an array cannot be negative"))
    let data = Array.zeroCreate<byte> (Checked.(*) stype.Layout.RawDataSize length)
    let references = Array.zeroCreate<RuntimeObject> (Checked.(*) stype.Layout.ObjectReferencesLength length)

    member _.Length = length

    member private _.ElementLength arr = Array.length arr / length

    member private this.ElementData i = OffsetArray<byte>(i * this.ElementLength data, data)

    member private this.ElementReferences i = OffsetArray<RuntimeObject>(i * this.ElementLength references, references)

    member this.Item
        with get index = { RuntimeStruct.RawData = this.ElementData index; References = this.ElementReferences index }
        and set index (value: RuntimeStruct) =
            value.RawData.ToSpan().CopyTo(Span(data, index * this.ElementLength data, value.RawData.Length))
            value.References.ToSpan().CopyTo(Span(references, index * this.ElementLength references, value.References.Length))

[<RequireQualifiedAccess; NoComparison; ReferenceEquality>]
type RuntimeObject =
    | Null
    /// Represents a class instance or a boxed value type.
    | TypeInstance of otype: RuntimeTypeDefinition * fields: RuntimeStruct
    | ByteVector of elements: uint8[]
    | ShortVector of elements: uint16[]
    | IntVector of elements: uint32[]
    | LongVector of elements: uint64[]
    | NativeIntVector of elements: unativeint[]
    | StructVector of elements: StructVector
    | ObjectVector of elements: RuntimeObject[]

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type RuntimeStruct =
    { RawData: OffsetArray<byte>; References: OffsetArray<RuntimeObject> }

    member this.ReadRaw<'T when 'T : struct and 'T :> System.ValueType and 'T : (new: unit -> 'T)> index: 'T =
        MemoryMarshal.Read<'T>(Span.op_Implicit(this.RawData.ToSpan()).Slice(index))

    member this.WriteRaw<'T when 'T : struct and 'T :> System.ValueType and 'T : (new: unit -> 'T)>(index, value: 'T) =
        let mutable value = value
        MemoryMarshal.Write<'T>(this.RawData.ToSpan().Slice(index), &value)

    member this.ReadRef index = this.References.ToSpan().[index]

    member this.WriteRef(index, o) = this.References.ToSpan().[index] <- o

[<Sealed>]
type MissingReturnInstructionException (frame, message) = inherit RuntimeException(frame, message)

[<Sealed>]
type InvalidComparisonException (message) = inherit RuntimeException(ValueNone, message)

[<RequireQualifiedAccess>]
module Interpreter =
    open UByte.Format.Model.InstructionSet

    let private copyRegisterValues (source: ImmutableArray<RuntimeRegister>) (dest: ImmutableArray<RuntimeRegister>) =
        if source.Length > dest.Length then failwith "TODO: Error, more source registers than destination registers" // TODO: Have validation of arguments lengths be the caller's problem.
        for i = 0 to dest.Length - 1 do source.[i].CopyValueTo dest.[i]

    let private numericFieldAccess frame field =
        FieldAccessException (
            ValueSome frame,
            "Attempted to access the field of a field of a numeric value",
            field
        )
        |> raise

    let private nullReferenceFieldAccess frame field =
        NullReferenceFieldAccessException (
            ValueSome frame,
            "Attempted to access an object field with a null object reference",
            field
        )
        |> raise

    let private nullReferenceArrayAccess frame =
        raise (RuntimeException(frame, "Attempted to access an array element with a null array reference"))

    /// Contains functions for retrieving numeric values from registers.
    module private NumberValue =
        let invalidNumericValue() = failwith "TODO: Error for cannot convert object or struct to numeric value"

        let f32 register =
            match register with
            | RuntimeRegister.U8 { contents = Cast.F32 value }
            | RuntimeRegister.S8 { contents = Cast.F32 value }
            | RuntimeRegister.U16 { contents = Cast.F32 value }
            | RuntimeRegister.S16 { contents = Cast.F32 value }
            | RuntimeRegister.U32 { contents = Cast.F32 value }
            | RuntimeRegister.S32 { contents = Cast.F32 value }
            | RuntimeRegister.S64 { contents = Cast.F32 value }
            | RuntimeRegister.U64 { contents = Cast.F32 value }
            | RuntimeRegister.SNative { contents = Cast.F32 value }
            | RuntimeRegister.UNative { contents = Cast.F32 value }
            | RuntimeRegister.F32 { contents = value }
            | RuntimeRegister.F64 { contents = Cast.F32 value } -> value
            | RuntimeRegister.Object _ | RuntimeRegister.Struct _ -> invalidNumericValue()

        let f64 register =
            match register with
            | RuntimeRegister.U8 { contents = Cast.F64 value }
            | RuntimeRegister.S8 { contents = Cast.F64 value }
            | RuntimeRegister.U16 { contents = Cast.F64 value }
            | RuntimeRegister.S16 { contents = Cast.F64 value }
            | RuntimeRegister.U32 { contents = Cast.F64 value }
            | RuntimeRegister.S32 { contents = Cast.F64 value }
            | RuntimeRegister.S64 { contents = Cast.F64 value }
            | RuntimeRegister.U64 { contents = Cast.F64 value }
            | RuntimeRegister.SNative { contents = Cast.F64 value }
            | RuntimeRegister.UNative { contents = Cast.F64 value }
            | RuntimeRegister.F32 { contents = Cast.F64 value }
            | RuntimeRegister.F64 { contents = value } -> value
            | RuntimeRegister.Object _ | RuntimeRegister.Struct _ -> invalidNumericValue()

        let u16 register =
            match register with
            | RuntimeRegister.U8 { contents = Cast.U16 value }
            | RuntimeRegister.S8 { contents = Cast.U16 value }
            | RuntimeRegister.U16 { contents = value }
            | RuntimeRegister.S16 { contents = Cast.U16 value }
            | RuntimeRegister.U32 { contents = Cast.U16 value }
            | RuntimeRegister.S32 { contents = Cast.U16 value }
            | RuntimeRegister.S64 { contents = Cast.U16 value }
            | RuntimeRegister.U64 { contents = Cast.U16 value }
            | RuntimeRegister.SNative { contents = Cast.U16 value }
            | RuntimeRegister.UNative { contents = Cast.U16 value }
            | RuntimeRegister.F32 { contents = Cast.U16 value }
            | RuntimeRegister.F64 { contents = Cast.U16 value } -> value
            | RuntimeRegister.Object _ | RuntimeRegister.Struct _ -> invalidNumericValue()

        let s16 register =
            match register with
            | RuntimeRegister.U8 { contents = Cast.S16 value }
            | RuntimeRegister.S8 { contents = Cast.S16 value }
            | RuntimeRegister.U16 { contents = Cast.S16 value }
            | RuntimeRegister.S16 { contents = value }
            | RuntimeRegister.U32 { contents = Cast.S16 value }
            | RuntimeRegister.S32 { contents = Cast.S16 value }
            | RuntimeRegister.S64 { contents = Cast.S16 value }
            | RuntimeRegister.U64 { contents = Cast.S16 value }
            | RuntimeRegister.SNative { contents = Cast.S16 value }
            | RuntimeRegister.UNative { contents = Cast.S16 value }
            | RuntimeRegister.F32 { contents = Cast.S16 value }
            | RuntimeRegister.F64 { contents = Cast.S16 value } -> value
            | RuntimeRegister.Object _ | RuntimeRegister.Struct _ -> invalidNumericValue()

        let u32 register =
            match register with
            | RuntimeRegister.U8 { contents = Cast.U32 value }
            | RuntimeRegister.S8 { contents = Cast.U32 value }
            | RuntimeRegister.U16 { contents = Cast.U32 value }
            | RuntimeRegister.S16 { contents = Cast.U32 value }
            | RuntimeRegister.U32 { contents = value }
            | RuntimeRegister.S32 { contents = Cast.U32 value }
            | RuntimeRegister.S64 { contents = Cast.U32 value }
            | RuntimeRegister.U64 { contents = Cast.U32 value }
            | RuntimeRegister.SNative { contents = Cast.U32 value }
            | RuntimeRegister.UNative { contents = Cast.U32 value }
            | RuntimeRegister.F32 { contents = Cast.U32 value }
            | RuntimeRegister.F64 { contents = Cast.U32 value } -> value
            | RuntimeRegister.Object _ | RuntimeRegister.Struct _ -> invalidNumericValue()

        let s32 register =
            match register with
            | RuntimeRegister.U8 { contents = Cast.S32 value }
            | RuntimeRegister.S8 { contents = Cast.S32 value }
            | RuntimeRegister.U16 { contents = Cast.S32 value }
            | RuntimeRegister.S16 { contents = Cast.S32 value }
            | RuntimeRegister.U32 { contents = Cast.S32 value }
            | RuntimeRegister.S32 { contents = value }
            | RuntimeRegister.S64 { contents = Cast.S32 value }
            | RuntimeRegister.U64 { contents = Cast.S32 value }
            | RuntimeRegister.SNative { contents = Cast.S32 value }
            | RuntimeRegister.UNative { contents = Cast.S32 value }
            | RuntimeRegister.F32 { contents = Cast.S32 value }
            | RuntimeRegister.F64 { contents = Cast.S32 value } -> value
            | RuntimeRegister.Object _ | RuntimeRegister.Struct _ -> invalidNumericValue()

        let u64 register =
            match register with
            | RuntimeRegister.U8 { contents = Cast.U64 value }
            | RuntimeRegister.S8 { contents = Cast.U64 value }
            | RuntimeRegister.U16 { contents = Cast.U64 value }
            | RuntimeRegister.S16 { contents = Cast.U64 value }
            | RuntimeRegister.U32 { contents = Cast.U64 value }
            | RuntimeRegister.S32 { contents = Cast.U64 value }
            | RuntimeRegister.S64 { contents = Cast.U64 value }
            | RuntimeRegister.U64 { contents = value }
            | RuntimeRegister.SNative { contents = Cast.U64 value }
            | RuntimeRegister.UNative { contents = Cast.U64 value }
            | RuntimeRegister.F32 { contents = Cast.U64 value }
            | RuntimeRegister.F64 { contents = Cast.U64 value } -> value
            | RuntimeRegister.Object _ | RuntimeRegister.Struct _ -> invalidNumericValue()

        let s64 register =
            match register with
            | RuntimeRegister.U8 { contents = Cast.S64 value }
            | RuntimeRegister.S8 { contents = Cast.S64 value }
            | RuntimeRegister.U16 { contents = Cast.S64 value }
            | RuntimeRegister.S16 { contents = Cast.S64 value }
            | RuntimeRegister.U32 { contents = Cast.S64 value }
            | RuntimeRegister.S32 { contents = Cast.S64 value }
            | RuntimeRegister.S64 { contents = value }
            | RuntimeRegister.U64 { contents = Cast.S64 value }
            | RuntimeRegister.SNative { contents = Cast.S64 value }
            | RuntimeRegister.UNative { contents = Cast.S64 value }
            | RuntimeRegister.F32 { contents = Cast.S64 value }
            | RuntimeRegister.F64 { contents = Cast.S64 value } -> value
            | RuntimeRegister.Object _ | RuntimeRegister.Struct _ -> invalidNumericValue()

        let unative register =
            match register with
            | RuntimeRegister.U8 { contents = Cast.UNative value }
            | RuntimeRegister.S8 { contents = Cast.UNative value }
            | RuntimeRegister.U16 { contents = Cast.UNative value }
            | RuntimeRegister.S16 { contents = Cast.UNative value }
            | RuntimeRegister.U32 { contents = Cast.UNative value }
            | RuntimeRegister.S32 { contents = Cast.UNative value }
            | RuntimeRegister.S64 { contents = Cast.UNative value }
            | RuntimeRegister.U64 { contents = Cast.UNative value }
            | RuntimeRegister.SNative { contents = Cast.UNative value }
            | RuntimeRegister.UNative { contents = value }
            | RuntimeRegister.F32 { contents = Cast.UNative value }
            | RuntimeRegister.F64 { contents = Cast.UNative value } -> value
            | RuntimeRegister.Object _ | RuntimeRegister.Struct _ -> invalidNumericValue()

        let snative register =
            match register with
            | RuntimeRegister.U8 { contents = Cast.SNative value }
            | RuntimeRegister.S8 { contents = Cast.SNative value }
            | RuntimeRegister.U16 { contents = Cast.SNative value }
            | RuntimeRegister.S16 { contents = Cast.SNative value }
            | RuntimeRegister.U32 { contents = Cast.SNative value }
            | RuntimeRegister.S32 { contents = Cast.SNative value }
            | RuntimeRegister.S64 { contents = Cast.SNative value }
            | RuntimeRegister.U64 { contents = Cast.SNative value }
            | RuntimeRegister.SNative { contents = value }
            | RuntimeRegister.UNative { contents = Cast.SNative value }
            | RuntimeRegister.F32 { contents = Cast.SNative value }
            | RuntimeRegister.F64 { contents = Cast.SNative value } -> value
            | RuntimeRegister.Object _ | RuntimeRegister.Struct _ -> invalidNumericValue()

    /// Contains functions for performing arithmetic on the values stored in registers.
    [<RequireQualifiedAccess>]
    module private Arithmetic =
        // Performs an operation on two integers and stores a result value, with no overflow checks.
        let inline private binop opu8 ops8 opu16 ops16 opu32 ops32 opu64 ops64 opunative opsnative opf32 opf64 xreg yreg rreg =
            match rreg with
            | RuntimeRegister.U32 r -> r.contents <- opu32 (NumberValue.u32 xreg) (NumberValue.u32 yreg)
            | RuntimeRegister.S32 r -> r.contents <- ops32 (NumberValue.s32 xreg) (NumberValue.s32 yreg)
            | RuntimeRegister.U64 r -> r.contents <- opu64 (NumberValue.u64 xreg) (NumberValue.u64 yreg)
            | RuntimeRegister.S64 r -> r.contents <- ops64 (NumberValue.s64 xreg) (NumberValue.s64 yreg)
            | RuntimeRegister.F32 r -> r.contents <- opf32 (NumberValue.f32 xreg) (NumberValue.f32 yreg)
            | RuntimeRegister.F64 r -> r.contents <- opf64 (NumberValue.f64 xreg) (NumberValue.f64 yreg)

        let inline private unop opu8 ops8 opu16 ops16 opu32 ops32 opu64 ops64 opunative opsnative opf32 opf64 register =
            match register with
            | RuntimeRegister.S8 r -> r.contents <- ops8 r.contents
            | RuntimeRegister.U8 r -> r.contents <- opu8 r.contents
            | RuntimeRegister.S16 r -> r.contents <- ops16 r.contents
            | RuntimeRegister.U16 r -> r.contents <- opu16 r.contents
            | RuntimeRegister.S32 r -> r.contents <- ops32 r.contents
            | RuntimeRegister.U32 r -> r.contents <- opu32 r.contents
            | RuntimeRegister.S64 r -> r.contents <- ops64 r.contents
            | RuntimeRegister.U64 r -> r.contents <- opu64 r.contents
            | RuntimeRegister.SNative r -> r.contents <- opsnative r.contents
            | RuntimeRegister.UNative r -> r.contents <- opunative r.contents
            | RuntimeRegister.F32 r -> r.contents <- opf32 r.contents
            | RuntimeRegister.F64 r -> r.contents <- opf64 r.contents

        let add xreg yreg rreg = binop (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) xreg yreg rreg
        let sub xreg yreg rreg = binop (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) xreg yreg rreg
        let mul xreg yreg rreg = binop (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) xreg yreg rreg

        let private bitf32 operation = fun (Reinterpret.F32 x) (Reinterpret.F32 y) -> Reinterpret.(|U32|) (operation x y)
        let private bitf64 operation = fun (Reinterpret.F64 x) (Reinterpret.F64 y) -> Reinterpret.(|U64|) (operation x y)
        let ``and`` xreg yreg rreg = binop (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (bitf32 (&&&)) (bitf64 (&&&)) xreg yreg rreg
        let ``or`` xreg yreg rreg = binop (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) (bitf32 (|||)) (bitf64 (|||)) xreg yreg rreg
        //let ``not`` xreg yreg rreg = unop (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) xreg yreg rreg
        let xor xreg yreg rreg = binop (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (bitf32 (^^^)) (bitf64 (^^^)) xreg yreg rreg

        let inline private oneop (op: _ -> _ -> _) = op LanguagePrimitives.GenericOne
        let inline private increment value = oneop (+) value
        let incr reg = unop increment increment increment increment increment increment increment increment increment increment increment increment reg
        let inline private decrement value = oneop (-) value
        let decr reg = unop decrement decrement decrement decrement decrement decrement decrement decrement decrement decrement decrement decrement reg

        [<RequireQualifiedAccess>]
        module Checked =
            open Microsoft.FSharp.Core.Operators.Checked

            let add xreg yreg rreg = binop (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) xreg yreg rreg
            let sub xreg yreg rreg = binop (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) xreg yreg rreg
            let mul xreg yreg rreg = binop (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) xreg yreg rreg
            let inline private increment value = oneop (+) value
            let incr reg = unop increment increment increment increment increment increment increment increment increment increment increment increment reg
            let inline private decrement value = oneop (-) value
            let decr reg = unop decrement decrement decrement decrement decrement decrement decrement decrement decrement decrement decrement decrement reg

    /// Contains functions for comparing the values stored in registers.
    [<RequireQualifiedAccess>]
    module private Compare =
        let inline private comparison s8 u8 s16 u16 s32 u32 s64 u64 snative unative f32 f64 obj xreg yreg =
            match xreg, yreg with
            | RuntimeRegister.Object { contents = x }, RuntimeRegister.Object { contents = y } -> obj x y
            | RuntimeRegister.Object _, _ | _, RuntimeRegister.Object _ ->
                failwith "TODO: Error for comparisons between objects and numeric values are prohibited"
            | RuntimeRegister.Struct _, _ | _, RuntimeRegister.Struct _ ->
                raise(NotImplementedException "Comparisons of structs may be implemented in the future")
            | RuntimeRegister.F64 { contents = x }, _ -> f64 x (NumberValue.f64 yreg)
            | _, RuntimeRegister.F64 { contents = y } -> f64 (NumberValue.f64 xreg) y
            | RuntimeRegister.F32 { contents = x }, _ -> f32 x (NumberValue.f32 yreg)
            | _, RuntimeRegister.F32 { contents = y } -> f32 (NumberValue.f32 xreg) y
            | RuntimeRegister.SNative { contents = x }, RuntimeRegister.UNative { contents = y } ->
                if x < 0n then failwith "TODO: How to compare long and ulong?" else unative (unativeint x) y
            | RuntimeRegister.UNative { contents = x }, RuntimeRegister.SNative { contents = y } ->
                if y < 0n then failwith "TODO: How to compare long and ulong?" else unative x (unativeint y)
            | RuntimeRegister.UNative { contents = x }, _ -> unative x (NumberValue.unative yreg)
            | _, RuntimeRegister.UNative { contents = y } -> unative (NumberValue.unative xreg) y
            | RuntimeRegister.SNative { contents = x }, _ -> snative x (NumberValue.snative yreg)
            | _, RuntimeRegister.SNative { contents = y } -> snative (NumberValue.snative xreg) y
            | RuntimeRegister.S64 { contents = x }, RuntimeRegister.U64 { contents = y } ->
                if x < 0L then failwith "TODO: How to compare long and ulong?" else u64 (uint64 x) y
            | RuntimeRegister.U64 { contents = x }, RuntimeRegister.S64 { contents = y } ->
                if y < 0L then failwith "TODO: How to compare long and ulong?" else u64 x (uint64 y)
            | RuntimeRegister.U64 { contents = x }, _ -> u64 x (NumberValue.u64 yreg)
            | _, RuntimeRegister.U64 { contents = y } -> u64 (NumberValue.u64 xreg) y
            | RuntimeRegister.S64 { contents = x }, _ -> s64 x (NumberValue.s64 yreg)
            | _, RuntimeRegister.S64 { contents = y } -> s64 (NumberValue.s64 xreg) y
            | RuntimeRegister.S32 { contents = Cast.S64 x }, RuntimeRegister.U32 { contents = Cast.S64 y }
            | RuntimeRegister.U32 { contents = Cast.S64 x }, RuntimeRegister.S32 { contents = Cast.S64 y } -> s64 x y
            | RuntimeRegister.U32 { contents = x }, _ -> u32 x (NumberValue.u32 yreg)
            | _, RuntimeRegister.U32 { contents = y } -> u32 (NumberValue.u32 xreg) y
            | RuntimeRegister.S32 { contents = x }, _ -> s32 x (NumberValue.s32 yreg)
            | _, RuntimeRegister.S32 { contents = y } -> s32 (NumberValue.s32 xreg) y
            | RuntimeRegister.S16 { contents = Cast.S32 x }, RuntimeRegister.U16 { contents = Cast.S32 y }
            | RuntimeRegister.U16 { contents = Cast.S32 x }, RuntimeRegister.S16 { contents = Cast.S32 y } -> s32 x y
            | RuntimeRegister.U16 { contents = x }, _ -> u16 x (NumberValue.u16 yreg)
            | _, RuntimeRegister.U16 { contents = y } -> u16 (NumberValue.u16 xreg) y
            | RuntimeRegister.S16 { contents = x }, _ -> s16 x (NumberValue.s16 yreg)
            | _, RuntimeRegister.S16{ contents = y } -> s16 (NumberValue.s16 xreg) y
            | RuntimeRegister.S8 { contents = Cast.S16 x }, RuntimeRegister.U8 { contents = Cast.S16 y }
            | RuntimeRegister.U8 { contents = Cast.S16 x }, RuntimeRegister.S8 { contents = Cast.S16 y } -> s16 x y
            | RuntimeRegister.S8 { contents = x }, RuntimeRegister.S8 { contents = y } -> s8 x y
            | RuntimeRegister.U8 { contents = x }, RuntimeRegister.U8 { contents = y } -> u8 x y

        let isTrueValue register =
            match register with
            | RuntimeRegister.S8 { contents = 0y }
            | RuntimeRegister.U8 { contents = 0uy }
            | RuntimeRegister.S16 { contents = 0s }
            | RuntimeRegister.U16 { contents = 0us }
            | RuntimeRegister.S32 { contents = 0 }
            | RuntimeRegister.U32 { contents = 0u }
            | RuntimeRegister.S64 { contents = 0L }
            | RuntimeRegister.U64 { contents = 0UL }
            | RuntimeRegister.SNative { contents = 0n }
            | RuntimeRegister.UNative { contents = 0un }
            | RuntimeRegister.F32 { contents = 0.0f }
            | RuntimeRegister.F64 { contents = 0.0 }
            | RuntimeRegister.Object { contents = RuntimeObject.Null } -> false
            | RuntimeRegister.Struct _ -> failwith "TODO: How to determine if a struct is truthy"
            | _ -> true

        let inline isFalseValue register = not(isTrueValue register)

        let isLessThan xreg yreg = comparison (<) (<) (<) (<) (<) (<) (<) (<) (<) (<) (<) (<) (fun _ _ -> false) xreg yreg
        let isGreaterThan xreg yreg = comparison (>) (>) (>) (>) (>) (>) (>) (>) (>) (>) (>) (>) (fun _ _ -> false) xreg yreg
        let private refeq a b = Object.ReferenceEquals(a, b)
        let isEqual xreg yreg = comparison (=) (=) (=) (=) (=) (=) (=) (=) (=) (=) (=) (=) refeq xreg yreg
        let inline isNotEqual xreg yreg = not(isEqual xreg yreg)
        let isLessOrEqual xreg yreg = comparison (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) refeq xreg yreg
        let isGreaterOrEqual xreg yreg = comparison (>=) (>=) (>=) (>=) (>=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) refeq xreg yreg

    let private (|Registers|) (frame: RuntimeStackFrame) (registers: ImmutableArray<RegisterIndex>) =
        let mutable registers' = Array.zeroCreate registers.Length
        for i = 0 to registers.Length - 1 do registers'.[i] <- frame.RegisterAt registers.[i]
        Unsafe.As<RuntimeRegister[], ImmutableArray<RuntimeRegister>> &registers'

    let interpret returns arguments (entrypoint: RuntimeMethod) =
        let frame: RuntimeStackFrame voption ref = ref ValueNone
        let mutable runExternalCode: (RuntimeStackFrame voption ref -> unit) voption = ValueNone

        let inline invoke returns (arguments: ImmutableArray<_>) (method: RuntimeMethod) =
            method.SetupStackFrame(returns, frame, &runExternalCode)
            let arguments' = frame.contents.Value.ArgumentRegisters
            if arguments.Length <> arguments'.Length then
                failwithf "TODO: Error for argument array lengths do not match, expected %i got %i" arguments'.Length arguments.Length
            copyRegisterValues arguments arguments'

        invoke returns arguments entrypoint

        let inline cont() =
            match frame.contents with
            | ValueSome frame'-> frame'.InstructionIndex < frame'.Instructions.Length
            | ValueNone -> false

        while cont() do
            let frame' = frame.contents.Value

            let inline (|Register|) rindex = frame'.RegisterAt rindex
            let inline (|Method|) mindex: RuntimeMethod = frame'.CurrentMethod.Module.InitializeMethod mindex
            let inline (|Field|) findex: RuntimeField = frame'.CurrentMethod.Module.InitializeField findex
            let inline (|TypeSignature|) tindex: AnyType = frame'.CurrentMethod.Module.TypeSignatureAt tindex
            let inline (|Data|) dindex: ImmutableArray<_> = frame'.CurrentMethod.Module.DataAt dindex
            let inline (|BranchTarget|) (target: InstructionOffset) = Checked.(-) (Checked.(+) frame'.InstructionIndex target) 1

            let inline fieldAccessInstruction field object access =
                match object with
                | RuntimeRegister.Object { contents = RuntimeObject.Null } -> nullReferenceFieldAccess frame' field
                | RuntimeRegister.Object { contents = RuntimeObject.TypeInstance(_, data) }
                | RuntimeRegister.Struct data -> access data
                | RuntimeRegister.Object { contents = _ } ->
                    failwith "TODO: Error when attempted to access object field using reference to array"
                | _ -> numericFieldAccess frame' field

            let inline arrayAccessInstruction array index accu8 accu16 accu32 accu64 accun accstr accobj =
                let index' =
                    match index with
                    | RuntimeRegister.S32 { contents = i } -> i
                    | _ -> failwith "TODO: Convert to int32 and check bounds when getting index to array element"
                match array with
                | RuntimeRegister.Object { contents = RuntimeObject.Null } -> nullReferenceArrayAccess frame.contents
                | RuntimeRegister.Object { contents = RuntimeObject.ByteVector array' } -> accu8 array' index'
                | RuntimeRegister.Object { contents = RuntimeObject.ShortVector array' } -> accu16 array' index'
                | RuntimeRegister.Object { contents = RuntimeObject.IntVector array' } -> accu32 array' index'
                | RuntimeRegister.Object { contents = RuntimeObject.LongVector array' } -> accu64 array' index'
                | RuntimeRegister.Object { contents = RuntimeObject.NativeIntVector array' } -> accun array' index'
                | RuntimeRegister.Object { contents = RuntimeObject.StructVector array' } -> accstr array' index'
                | RuntimeRegister.Object { contents = RuntimeObject.ObjectVector array' } -> accobj array' index'
                | _ ->
                    failwith "TODO: Error for expected object reference to array but got value type"

            (*
            match ex with
            | ValueSome e ->
                failwith "TODO: Lookup exception handlers"
            | ValueNone -> ()
            *)

            try
                match frame'.Instructions.[frame'.InstructionIndex] with
                | Reg_copy(source, dest) -> (frame'.RegisterAt source).CopyValueTo(frame'.RegisterAt dest)
                | Add(Register x, Register y, Register r) -> Arithmetic.add x y r
                | Sub(Register x, Register y, Register r) -> Arithmetic.sub x y r
                | Mul(Register x, Register y, Register r) -> Arithmetic.mul x y r
                | And(Register x, Register y, Register r) -> Arithmetic.``and`` x y r
                | Or(Register x, Register y, Register r) -> Arithmetic.``or`` x y r
                | Xor(Register x, Register y, Register r) -> Arithmetic.xor x y r
                | Incr(Register register) -> Arithmetic.incr register
                | Decr(Register register) -> Arithmetic.decr register
                | Add_ovf(Register x, Register y, Register r) -> Arithmetic.Checked.add x y r
                | Sub_ovf(Register x, Register y, Register r) -> Arithmetic.Checked.sub x y r
                | Mul_ovf(Register x, Register y, Register r) -> Arithmetic.Checked.mul x y r
                | Incr_ovf(Register register) -> Arithmetic.Checked.incr register
                | Decr_ovf(Register register) -> Arithmetic.Checked.decr register
                | Const_i32(value, Register dest) -> Const.i32 (uint32 value) dest
                | Const_true(Register dest) -> Const.``true`` dest
                | Const_false(Register dest)
                | Const_zero(Register dest) -> Const.``false`` dest
                | Ret(Registers frame' registers) ->
                    copyRegisterValues registers frame'.ReturnRegisters
                    frame.contents <- frame'.Previous
                | Call(Method method, Registers frame' aregs, Registers frame' rregs) ->
                    invoke rregs aregs method
                | Call_ret(Method method, Registers frame' aregs, _) -> // TODO: Does call.ret need to specify return values?
                    frame.contents <- frame'.Previous
                    invoke frame'.ReturnRegisters aregs method
                | Br(BranchTarget target) -> frame'.InstructionIndex <- target
                | Br_eq(Register xreg, Register yreg, BranchTarget target) ->
                    if Compare.isEqual xreg yreg then frame'.InstructionIndex <- target
                | Br_ne(Register xreg, Register yreg, BranchTarget target) ->
                    if Compare.isNotEqual xreg yreg then frame'.InstructionIndex <- target
                | Br_lt(Register xreg, Register yreg, BranchTarget target) ->
                    if Compare.isLessThan xreg yreg then frame'.InstructionIndex <- target
                | Br_gt(Register xreg, Register yreg, BranchTarget target) ->
                    if Compare.isGreaterThan xreg yreg then frame'.InstructionIndex <- target
                | Br_le(Register xreg, Register yreg, BranchTarget target) ->
                    if Compare.isLessOrEqual xreg yreg then frame'.InstructionIndex <- target
                | Br_ge(Register xreg, Register yreg, BranchTarget target) ->
                    if Compare.isGreaterOrEqual xreg yreg then frame'.InstructionIndex <- target
                | Br_true(Register register, BranchTarget target) ->
                    if Compare.isTrueValue register then frame'.InstructionIndex <- target
                | Br_false(Register register, BranchTarget target) ->
                    if Compare.isFalseValue register then frame'.InstructionIndex <- target
                | Obj_null(Register destination) -> Const.obj RuntimeObject.Null destination
                | Obj_new(Method constructor, Registers frame' arguments, Register destination) ->
                    match destination with
                    | RuntimeRegister.Object o ->
                        o.contents <- RuntimeObject.TypeInstance(constructor.DeclaringType, constructor.DeclaringType.InitializeObjectFields())
                        // TODO: Check that first argument is an object reference.
                        invoke ImmutableArray.Empty (arguments.Insert(0, destination)) constructor
                    | bad ->
                        failwithf "TODO: Error for cannot store object reference here after calling constructor %A" bad
                | Obj_ldfd(Field field, Register object, Register destination) ->
                    field.CheckMutate frame'

                    fieldAccessInstruction field object <| fun fields ->
                        match destination with // TODO: How to respect endianness when reading values from struct
                        | RuntimeRegister.U8 destination' ->
                            destination'.contents <- fields.ReadRaw<uint8> field.Offset
                        | RuntimeRegister.U16 destination' ->
                            destination'.contents <- fields.ReadRaw<uint16> field.Offset
                        | RuntimeRegister.U32 destination' ->
                            destination'.contents <- fields.ReadRaw<uint32> field.Offset
                        | RuntimeRegister.U64 destination' ->
                            destination'.contents <- fields.ReadRaw<uint64> field.Offset
                        | RuntimeRegister.UNative destination' ->
                            destination'.contents <- fields.ReadRaw<unativeint> field.Offset
                        | RuntimeRegister.S8 destination' ->
                            destination'.contents <- fields.ReadRaw<int8> field.Offset
                        | RuntimeRegister.S16 destination' ->
                            destination'.contents <- fields.ReadRaw<int16> field.Offset
                        | RuntimeRegister.S32 destination' ->
                            destination'.contents <- fields.ReadRaw<int32> field.Offset
                        | RuntimeRegister.S64 destination' ->
                            destination'.contents <- fields.ReadRaw<int64> field.Offset
                        | RuntimeRegister.SNative destination' ->
                            destination'.contents <- fields.ReadRaw<nativeint> field.Offset
                        | RuntimeRegister.F32 destination' ->
                            destination'.contents <- fields.ReadRaw<single> field.Offset
                        | RuntimeRegister.F64 destination' ->
                            destination'.contents <- fields.ReadRaw<double> field.Offset
                        | RuntimeRegister.Struct _ ->
                            failwith "// TODO: How to read structs stored inside other structs?"
                        | RuntimeRegister.Object destination' ->
                            destination'.contents <- fields.ReadRef(field.Offset)
                | Obj_stfd(Field field, Register object, Register source) ->
                    fieldAccessInstruction field object <| fun fields ->
                        match source with
                        | RuntimeRegister.S8 { contents = Cast.U8 value }
                        | RuntimeRegister.U8 { contents = value } -> fields.WriteRaw(field.Offset, value)
                        | RuntimeRegister.S16 { contents = Cast.U16 value }
                        | RuntimeRegister.U16 { contents = value } -> fields.WriteRaw(field.Offset, value)
                        | RuntimeRegister.S32 { contents = Cast.U32 value }
                        | RuntimeRegister.F32 { contents = Reinterpret.F32 value }
                        | RuntimeRegister.U32 { contents = value } -> fields.WriteRaw(field.Offset, value)
                        | RuntimeRegister.S64 { contents = Cast.U64 value }
                        | RuntimeRegister.F64 { contents = Reinterpret.F64 value }
                        | RuntimeRegister.U64 { contents = value } -> fields.WriteRaw(field.Offset, value)
                        | RuntimeRegister.UNative { contents = value } -> fields.WriteRaw(field.Offset, value)
                        | RuntimeRegister.Struct _ ->
                            failwith "// TODO: How to store structs stored inside other structs?"
                        | RuntimeRegister.Object { contents = value } -> fields.WriteRef(field.Offset, value)
                | Obj_arr_new(TypeSignature etype, Register length, Register destination) ->
                    let length' =
                        match length with
                        | RuntimeRegister.S32 { contents = value } -> value

                    match destination with
                    | RuntimeRegister.Object destination' ->
                        match etype with
                        | ValueType vt ->
                            match vt with
                            | ValueType.Primitive PrimitiveType.Char32 ->
                                destination'.contents <- RuntimeObject.IntVector(Array.zeroCreate length')
                        | ReferenceType _ ->
                            destination'.contents <- RuntimeObject.ObjectVector(Array.zeroCreate length')
                        | SafePointer _ ->
                            failwith "TODO: Error for cannont instantiate array containing safe pointers"
                    | _ -> failwith "TODO: Error for cannot store object reference to array here"
                | Obj_arr_const(TypeSignature etype, Data data, Register destination) ->
                    match destination with
                    | RuntimeRegister.Object destination' ->
                        match etype with
                        | ValueType vt ->
                            match vt with
                            | ValueType.Primitive PrimitiveType.S8
                            | ValueType.Primitive PrimitiveType.U8 ->
                                destination'.contents <- RuntimeObject.ByteVector(data.AsSpan().ToArray())
                            | ValueType.Primitive PrimitiveType.S32
                            | ValueType.Primitive PrimitiveType.U32
                            | ValueType.Primitive PrimitiveType.Char32 ->
                                destination'.contents <- RuntimeObject.IntVector(MemoryMarshal.Cast<uint8, uint32>(data.AsSpan()).ToArray())
                        | _ ->
                            failwith "TODO: Error for cannot create data array containing object references or safe pointers"
                    | _ -> failwith "TODO: Error for cannot store object reference to data array here"
                | Obj_arr_len(Register array, Register length) ->
                    // TODO: Make common function for assuming register contains object reference
                    match array with
                    | RuntimeRegister.Object { contents = o } ->
                        let inline (|Length|) array = ((^T) : (member Length : int32) array)
                        match o with
                        | RuntimeObject.ByteVector(Length len)
                        | RuntimeObject.ShortVector(Length len)
                        | RuntimeObject.IntVector(Length len)
                        | RuntimeObject.LongVector(Length len)
                        | RuntimeObject.NativeIntVector(Length len)
                        | RuntimeObject.StructVector(Length len)
                        | RuntimeObject.ObjectVector(Length len) ->
                            Const.i32 (uint32 len) length
                        | _ -> failwith "TODO: Error for cannot get length of array when object reference is not to an array"
                    | _ -> failwith "TODO: Error for expected object reference to array but got value type"
                | Obj_arr_get(Register array, Register index, Register destination) ->
                    arrayAccessInstruction array index
                        (fun _ _ -> failwith "TODO: Array u8 element not supported")
                        (fun _ _ -> failwith "TODO: Array u16 element not supported")
                        (fun array i -> Const.i32 array.[i] destination)
                        (fun _ _ -> failwith "TODO: Array u64 element not supported")
                        (fun _ _ -> failwith "TODO: Array unative element not supported")
                        (fun _ _ -> failwith "TODO: Array struct element not supported")
                        (fun array i -> RuntimeRegister.Object(ref array.[i]).CopyValueTo(destination)) // TODO: Define a Const.obj function instead
                | Obj_arr_set(Register array, Register index, Register source) ->
                    arrayAccessInstruction array index
                        (fun _ _ -> failwith "TODO: Array u8 element not supported")
                        (fun _ _ -> failwith "TODO: Array u16 element not supported")
                        (fun array' i -> array'.[i] <- NumberValue.u32 source)
                        (fun _ _ -> failwith "TODO: Array u64 element not supported")
                        (fun _ _ -> failwith "TODO: Array unative element not supported")
                        (fun _ _ -> failwith "TODO: Array struct element not supported")
                        (fun _ i -> failwith "TODO Array object element not supported")
                | Nop -> ()
                | bad -> failwithf "TODO: Unsupported instruction %A" bad

                match runExternalCode with
                | ValueNone -> ()
                | ValueSome run ->
                    run frame
                    runExternalCode <- ValueNone

                frame'.InstructionIndex <- Checked.(+) frame'.InstructionIndex 1
            with
            | e ->
                //ex <- ValueSome e
                raise(System.NotImplementedException("TODO: Implement exception handling: " + Environment.NewLine + frame'.StackTrace, e))

        match frame.contents with
        | ValueNone -> ()
        | ValueSome _ -> raise(MissingReturnInstructionException(frame.contents, "Reached unexpected end of instructions"))

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
        match frame.ArgumentRegisters.[0] with
        | RuntimeRegister.Object { contents = RuntimeObject.Null } -> stdout.WriteLine()
        | RuntimeRegister.Object { contents = RuntimeObject.IntVector chars } ->
            for c in chars do stdout.Write(System.Text.Rune(c).ToString())
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
type RuntimeMethod (rmodule: RuntimeModule, method: Method) =
    let { Method.MethodFlags = flags; Body = body } = method

    member _.Module: RuntimeModule = rmodule

    member val Name = rmodule.IdentifierAt method.MethodName

    member val Visibility = method.MethodVisibility

    member val DeclaringType = rmodule.InitializeType method.MethodOwner

    member val Signature = rmodule.MethodSignatureAt method.Signature

    member _.IsInstance = isFlagSet MethodFlags.Instance flags

    member _.IsConstructor = isFlagSet MethodFlags.ConstructorMask flags

    member private _.CreateRegister rtype =
        match rmodule.TypeSignatureAt rtype with
        | ValueType vt ->
            match vt with
            | ValueType.Primitive PrimitiveType.S8 -> fun() -> RuntimeRegister.S8(ref 0y)
            | ValueType.Primitive PrimitiveType.Bool
            | ValueType.Primitive PrimitiveType.U8 -> fun() -> RuntimeRegister.U8(ref 0uy)
            | ValueType.Primitive PrimitiveType.S16 -> fun() -> RuntimeRegister.S16(ref 0s)
            | ValueType.Primitive PrimitiveType.U16
            | ValueType.Primitive PrimitiveType.Char16 -> fun() -> RuntimeRegister.U16(ref 0us)
            | ValueType.Primitive PrimitiveType.S32 -> fun() -> RuntimeRegister.S32(ref 0)
            | ValueType.Primitive PrimitiveType.U32
            | ValueType.Primitive PrimitiveType.Char32 -> fun() -> RuntimeRegister.U32(ref 0u)
            | ValueType.Primitive PrimitiveType.S64 -> fun() -> RuntimeRegister.S64(ref 0L)
            | ValueType.Primitive PrimitiveType.U64 -> fun() -> RuntimeRegister.U64(ref 0UL)
            | ValueType.Primitive PrimitiveType.F32 -> fun() -> RuntimeRegister.F32(ref 0.0f)
            | ValueType.Primitive PrimitiveType.F64 -> fun() -> RuntimeRegister.F64(ref 0.0)
            | ValueType.Primitive PrimitiveType.SNative -> fun() -> RuntimeRegister.SNative(ref 0n)
            | ValueType.Primitive PrimitiveType.UNative
            | ValueType.UnsafePointer _ -> fun() -> RuntimeRegister.UNative(ref 0un)
            | ValueType.Primitive PrimitiveType.Unit -> fun() -> failwith "TODO: Prevent usage of Unit in register types."
            | ValueType.Defined _ -> failwith "TODO: Add support for registers containing structs"
        | ReferenceType _ -> fun() -> RuntimeRegister.Object(ref RuntimeObject.Null)
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

            let registers =
                let registers = ImmutableArray.CreateBuilder()
                for struct(count, rtype) in code.RegisterTypes do
                    let create = this.CreateRegister rtype.RegisterType
                    for _ = 1 to Checked.int32 count do create() |> registers.Add
                registers.ToImmutable()

            frame.contents <- ValueSome(RuntimeStackFrame(frame.contents, args, registers, returns, code.Instructions, this))
        | MethodBody.Abstract -> failwith "TODO: Handle virtual calls"
        | MethodBody.External(library, efunction) ->
            let library' = this.Module.IdentifierAt library
            let efunction' = this.Module.IdentifierAt efunction
            let args = this.CreateArgumentRegisters()
            let frame' = RuntimeStackFrame(frame.contents, args, ImmutableArray.Empty, returns, ImmutableArray.Empty, this)
            frame.contents <- ValueSome frame'
            runExternalCode <- ValueSome <| fun frame'' ->
                ExternalCode.call library' efunction' frame''.contents.Value
                frame''.contents <- frame'.Previous

    override this.ToString() = sprintf "%O.%s" this.DeclaringType this.Name // TODO: Include method signature in stack trace

[<Sealed>]
type RuntimeField (rmodule: RuntimeModule, field: Field, n: int32) =
    let { Field.FieldName = namei; FieldFlags = flags } = field

    do
        if isFlagSet FieldFlags.Static field.FieldFlags then raise(NotSupportedException "Static fields are not yet supported")

    member _.Module = rmodule

    member _.Name = rmodule.IdentifierAt namei

    member _.IsMutable = isFlagSet FieldFlags.Mutable flags

    member val DeclaringType: RuntimeTypeDefinition = rmodule.InitializeType field.FieldOwner

    member val FieldType = rmodule.TypeSignatureAt field.FieldType

    member this.Offset = this.DeclaringType.Layout.FieldIndices.[this] // TODO: Cache field offset

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
    { Fields: RuntimeField[]
      FieldIndices: IReadOnlyDictionary<RuntimeField, int32>
      RawDataSize: int32
      ObjectReferencesLength: int32 }

let emptyTypeLayout =
    lazy
        { RuntimeTypeLayout.Fields = Array.empty
          FieldIndices = ImmutableDictionary.Empty
          RawDataSize = 0
          ObjectReferencesLength = 0 }

[<Sealed>]
type RuntimeTypeDefinition (rm: RuntimeModule, t: TypeDefinition) =
    let layout =
        let { TypeDefinition.Fields = fields } = t
        if fields.Length > 0 then
            lazy
                let fields', indices = Array.zeroCreate fields.Length, Dictionary fields.Length
                let mutable sumDataSize, sumReferencesLength = 0, 0

                for i = 0 to fields'.Length - 1 do
                    // TODO: Check for static fields.
                    let mutable dsize, rlen = 0, 0
                    let rfield = rm.ComputeFieldSize(fields.[i], &dsize, &rlen)
                    fields'.[i] <- rfield

                    // Stores either the index into the data array or reference array, the kind of index depends on whether dsize
                    // or rlen was set, only one will be zero
                    indices.Add(rfield, if dsize > rlen then sumDataSize else sumReferencesLength)

                    sumDataSize <- sumDataSize + dsize
                    sumReferencesLength <- sumReferencesLength + dsize

                { RuntimeTypeLayout.Fields = fields'
                  FieldIndices = indices
                  RawDataSize = sumDataSize
                  ObjectReferencesLength = sumReferencesLength }
        else emptyTypeLayout

    //let globals: RuntimeStruct

    let methodis = t.Methods

    // TODO: Cache length of RawData and References arrays for RuntimeObject and RuntimeStruct

    // TODO: Cache list of all types that are inherited, make sure when list is created that there are no circular dependencies

    member _.Module = rm

    member _.Layout: RuntimeTypeLayout = layout.Value

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
            (fun i _ -> RuntimeMethod(rm, definitions.[i]))
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
            let n = owners.[Checked.int32 owner].Fields.IndexOf i'
            RuntimeField(rm, f, n)

    let typeNameLookup = Dictionary()

    member val Name = m.Header.Module.ModuleName.ToString()
    member val Version = m.Header.Module.Version

    member _.IdentifierAt(Index i: IdentifierIndex) = m.Identifiers.Identifiers.[Checked.int32 i]

    member this.NamespaceAt(Index i: NamespaceIndex): string =
        m.Namespaces.[Checked.int32 i] |> Seq.map this.IdentifierAt |> String.concat "::" // TODO: Cache namespaces

    member _.TypeSignatureAt(Index i: TypeSignatureIndex) = m.TypeSignatures.[Checked.int32 i]

    member _.MethodSignatureAt(Index i: MethodSignatureIndex) = m.MethodSignatures.[Checked.int32 i]

    member _.DataAt(Index i: DataIndex) = m.Data.[Checked.int32 i]

    member _.CodeAt(Index i: CodeIndex) = m.Code.[Checked.int32 i]

    member _.InitializeMethod i = definedMethodLookup i

    member _.InitializeField i = definedFieldLookup i

    member _.InitializeType i = typeDefinitionLookup i

    member _.ComputeFieldSize(index, rawDataSize: outref<_>, objectReferencesLength: outref<_>) =
        let field = definedFieldLookup index

        match field.FieldType with
        | ValueType vt ->
            rawDataSize <-
                match vt with
                | ValueType.Primitive PrimitiveType.Bool
                | ValueType.Primitive PrimitiveType.U8
                | ValueType.Primitive PrimitiveType.S8 -> 1
                | ValueType.Primitive PrimitiveType.U16
                | ValueType.Primitive PrimitiveType.S16
                | ValueType.Primitive PrimitiveType.Char16 -> 2
                | ValueType.Primitive PrimitiveType.U32
                | ValueType.Primitive PrimitiveType.S32
                | ValueType.Primitive PrimitiveType.F32
                | ValueType.Primitive PrimitiveType.Char32 -> 4
                | ValueType.Primitive PrimitiveType.U64
                | ValueType.Primitive PrimitiveType.S64
                | ValueType.Primitive PrimitiveType.F64 -> 8
                | ValueType.Primitive PrimitiveType.UNative
                | ValueType.Primitive PrimitiveType.SNative
                | ValueType.UnsafePointer _ -> sizeof<unativeint>
                | ValueType.Primitive PrimitiveType.Unit -> 0
                | ValueType.Defined _ -> failwith "TODO: Struct fields are currently not yet supported"
        | ReferenceType _ -> objectReferencesLength <- 1
        | SafePointer _ -> failwith "TODO: Error for fields cannot contain safe pointers"

        field

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
                    | AnyType.ReferenceType(ReferenceType.Vector tstring) ->
                        match tstring with
                        | ReferenceOrValueType.Reference(ReferenceType.Vector tchar) ->
                            let inline characterArrayArguments convert =
                                Array.init argv.Length (fun i -> convert argv.[i])
                                |> RuntimeObject.ObjectVector
                                |> ref
                                |> RuntimeRegister.Object
                                |> ImmutableArray.Create

                            match tchar with // TODO: Create a Char8 type for UTF-8 strings
                            | ReferenceOrValueType.Value(ValueType.Primitive PrimitiveType.Char16) ->
                                characterArrayArguments <| fun arg ->
                                    Array.init arg.Length (fun i -> uint16 arg.[i]) |> RuntimeObject.ShortVector
                            | ReferenceOrValueType.Value(ValueType.Primitive PrimitiveType.Char32) ->
                                let buffer = List()
                                characterArrayArguments <| fun arg ->
                                    buffer.Clear()
                                    for cu in arg.EnumerateRunes() do buffer.Add(uint32 cu.Value)
                                    buffer.ToArray() |> RuntimeObject.IntVector
                            | bad -> failwithf "TODO: Invalid character type %A" bad
                        | bad -> failwithf "TODO: Invalid string type %A" bad
                    | bad -> failwithf "TODO: Error for invalid entrypoint argument type %A" bad
                | _ -> failwith "TODO: Error for invalid number of arguments for entrypoint"

            let result = ref 0 // TODO: Check that entry point does not return more than 1 value.

            Interpreter.interpret (ImmutableArray.Create(RuntimeRegister.S32 result)) arguments main
            int32 result.contents
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
