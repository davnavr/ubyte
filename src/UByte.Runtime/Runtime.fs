module rec UByte.Interpreter.Runtime

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

open UByte.Format.Model

let inline isFlagSet flag value = value &&& flag = flag

[<RequireQualifiedAccess>]
type RuntimeRegister =
    | R1 of uint8 ref
    | R2 of uint16 ref
    | R4 of uint32 ref
    | R8 of uint64 ref
    | RNative of unativeint ref
    | RStruct of RuntimeStruct
    | RRef of RuntimeObject ref
    //| RSafePointer of _ -> _

    member source.CopyValueTo destination =
        match source, destination with
        | R1 { contents = value }, R1 dest -> dest.contents <- value
        | R1 { contents = value }, R2 dest -> dest.contents <- uint16 value
        | R1 { contents = value }, R4 dest -> dest.contents <- uint32 value
        | R1 { contents = value }, R8 dest -> dest.contents <- uint64 value
        | R2 { contents = value }, R2 dest -> dest.contents <- value
        | R2 { contents = value }, R4 dest -> dest.contents <- uint32 value
        | R2 { contents = value }, R8 dest -> dest.contents <- uint64 value
        | R4 { contents = value }, R4 dest -> dest.contents <- value
        | R4 { contents = value }, R8 dest -> dest.contents <- uint64 value
        | R8 { contents = value }, R8 dest -> dest.contents <- value
        | RNative { contents = value }, RNative dest -> dest.contents <- value
        | R2 _, R1 _
        | R4 _, R2 _
        | R4 _, R1 _
        | R8 _, R4 _
        | R8 _, R2 _
        | R8 _, R1 _ -> failwith "TODO: Truncating not yet supported"
        | RRef { contents = value }, RRef dest -> dest.contents <- value
        | RRef _, _
        | _, RRef _ -> failwith "TODO: Error for cannot mix integers and reference types"

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

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type OffsetArray<'T> =
    val Start: int32
    val Items: 'T[]

    new (start, items) = { Start = start; Items = items }
    new (items) = OffsetArray(0, items)
    new (length) = OffsetArray(Array.zeroCreate<'T> length)

    member this.Length = this.Items.Length - this.Start

    member this.ToSpan() = Span<'T>(this.Items).Slice this.Start

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

    [<RequireQualifiedAccess>]
    module private Arithmetic =
        let inline private unaryOp opu8 opu16 opu32 opu64 opunative register =
            match register with
            | RuntimeRegister.R1 r -> r.contents <- opu8 r.contents
            | RuntimeRegister.R2 r -> r.contents <- opu16 r.contents
            | RuntimeRegister.R4 r -> r.contents <- opu32 r.contents
            | RuntimeRegister.R8 r -> r.contents <- opu64 r.contents
            | RuntimeRegister.RNative r -> r.contents <- opunative r.contents

        let inline private binaryOp opu8 opu16 opu32 opu64 opunative xreg yreg rreg =
            match xreg, yreg, rreg with
            | RuntimeRegister.R1 { contents = x }, RuntimeRegister.R1 { contents = y }, RuntimeRegister.R1 result ->
                result.contents <- opu8 x y
            | RuntimeRegister.R2 { contents = x }, RuntimeRegister.R2 { contents = y }, RuntimeRegister.R2 result ->
                result.contents <- opu16 x y
            | RuntimeRegister.R4 { contents = x }, RuntimeRegister.R4 { contents = y }, RuntimeRegister.R4 result ->
                result.contents <- opu32 x y
            | RuntimeRegister.R8 { contents = x }, RuntimeRegister.R8 { contents = y }, RuntimeRegister.R8 result ->
                result.contents <- opu64 x y
            | RuntimeRegister.RNative { contents = x }, RuntimeRegister.RNative { contents = y }, RuntimeRegister.RNative result ->
                result.contents <- opunative x y

        let add xreg yreg rreg = binaryOp (+) (+) (+) (+) (+) xreg yreg rreg
        let sub xreg yreg rreg = binaryOp (-) (-) (-) (-) (-) xreg yreg rreg
        let mul xreg yreg rreg = binaryOp (*) (*) (*) (*) (*) xreg yreg rreg

        let ``and`` xreg yreg rreg = binaryOp (&&&) (&&&) (&&&) (&&&) (&&&) xreg yreg rreg
        let ``or`` xreg yreg rreg = binaryOp (|||) (|||) (|||) (|||) (|||) xreg yreg rreg
        //let ``not`` xreg yreg rreg = unaryOp (~~~) (~~~) (~~~) (~~~) (~~~) xreg yreg rreg
        let xor xreg yreg rreg = binaryOp (^^^) (^^^) (^^^) (^^^) (^^^) xreg yreg rreg
        let incr reg = unaryOp ((+) 1uy) ((+) 1us) ((+) 1u) ((+) 1UL) ((+) 1un) reg
        let decr reg = unaryOp ((-) 1uy) ((-) 1us) ((-) 1u) ((-) 1UL) ((-) 1un) reg

    [<RequireQualifiedAccess>]
    module private Const =
        let inline private store value destination =
            match destination with
            | RuntimeRegister.R1 dest' -> dest'.contents <- uint8 value
            | RuntimeRegister.R2 dest' -> dest'.contents <- uint16 value
            | RuntimeRegister.R4 dest' -> dest'.contents <- uint32 value
            | RuntimeRegister.R8 dest' -> dest'.contents <- uint64 value
            | RuntimeRegister.RNative dest' -> dest'.contents <- unativeint value
            //"Cannot store integer into a register containing an object reference"

        let i32 (value: int32) destination = store value destination
        let u8 (value: uint8) destination = store value destination
        let inline ``true`` destination = u8 1uy destination
        let inline ``false`` destination = u8 0uy destination

    [<RequireQualifiedAccess>]
    module private Compare =
        let inline comparison cu8 cu16 cu32 cu64 cunative cref xreg yreg =
            match xreg, yreg with
            | RuntimeRegister.R1 { contents = x }, RuntimeRegister.R1 { contents = y } -> cu8 x y
            | RuntimeRegister.R4 { contents = x }, RuntimeRegister.R4 { contents = y } -> cu32 x y

        let isTrueValue register =
            match register with
            | RuntimeRegister.R1 { contents = 0uy }
            | RuntimeRegister.R2 { contents = 0us }
            | RuntimeRegister.R4 { contents = 0u }
            | RuntimeRegister.R8 { contents = 0UL }
            | RuntimeRegister.RNative { contents = 0un }
            | RuntimeRegister.RRef { contents = RuntimeObject.Null } -> false
            | RuntimeRegister.RStruct _ -> failwith "TODO: How to determine if a struct is truthy"
            | _ -> true

        let inline isFalseValue register = not(isTrueValue register)

        let isLessThan xreg yreg = comparison (<) (<) (<) (<) (<) (fun _ _ -> false) xreg yreg
        let isGreaterThan xreg yreg = comparison (>) (>) (>) (>) (>) (fun _ _ -> false) xreg yreg
        let private refeq a b = Object.ReferenceEquals(a, b)
        let isEqual xreg yreg = comparison (=) (=) (=) (=) (=) refeq xreg yreg
        let inline isNotEqual xreg yreg = not(isEqual xreg yreg)
        let isLessOrEqual xreg yreg = comparison (<=) (<=) (<=) (<=) (<=) refeq xreg yreg
        let isGreaterOrEqual xreg yreg = comparison (>=) (>=) (>=) (>=) (>=) refeq xreg yreg

    let private (|Registers|) (frame: RuntimeStackFrame) (registers: ImmutableArray<RegisterIndex>) =
        let mutable registers' = Array.zeroCreate registers.Length
        for i = 0 to registers.Length - 1 do registers'.[i] <- frame.RegisterAt registers.[i]
        Unsafe.As<RuntimeRegister[], ImmutableArray<RuntimeRegister>> &registers'

    let interpret returns arguments (entrypoint: RuntimeMethod) =
        let mutable frame: RuntimeStackFrame voption = ValueNone

        let inline invoke returns (arguments: ImmutableArray<_>) (method: RuntimeMethod) =
            method.SetupStackFrame(returns, &frame)
            let arguments' = frame.Value.ArgumentRegisters
            if arguments.Length <> arguments'.Length then
                failwithf "TODO: Error for argument array lengths do not match, expected %i got %i" arguments'.Length arguments.Length
            copyRegisterValues arguments arguments'

        invoke returns arguments entrypoint

        let inline cont() =
            match frame with
            | ValueSome frame'-> frame'.InstructionIndex < frame'.Instructions.Length
            | ValueNone -> false

        while cont() do
            let frame' = frame.Value

            let inline (|Register|) rindex = frame'.RegisterAt rindex
            let inline (|Method|) mindex: RuntimeMethod = frame'.CurrentMethod.Module.InitializeMethod mindex
            let inline (|Field|) findex: RuntimeField = frame'.CurrentMethod.Module.InitializeField findex
            let inline (|TypeSignature|) tindex: AnyType = frame'.CurrentMethod.Module.TypeSignatureAt tindex
            let inline (|BranchTarget|) (target: InstructionOffset) = Checked.(-) (Checked.(+) frame'.InstructionIndex target) 1

            let inline fieldAccessInstruction field object access =
                match object with
                | RuntimeRegister.RRef { contents = RuntimeObject.Null } -> nullReferenceFieldAccess frame' field
                | RuntimeRegister.RRef { contents = RuntimeObject.TypeInstance(_, data) }
                | RuntimeRegister.RStruct data -> access data
                | RuntimeRegister.RRef { contents = _ } ->
                    failwith "TODO: Error when attempted to access object field using reference to array"
                | _ -> numericFieldAccess frame' field

            let inline arrayAccessInstruction array index accu8 accu16 accu32 accu64 accun accstr accobj =
                let index' =
                    match index with
                    | RuntimeRegister.R4 { contents = i } -> Checked.int32 i
                    | _ -> failwith "TODO: Convert to int32 and check bounds when getting index to array element"
                match array with
                | RuntimeRegister.RRef { contents = RuntimeObject.Null } -> nullReferenceArrayAccess frame
                | RuntimeRegister.RRef { contents = RuntimeObject.ByteVector array' } -> accu8 array' array'.[index']
                | RuntimeRegister.RRef { contents = RuntimeObject.ShortVector array' } -> accu16 array' array'.[index']
                | RuntimeRegister.RRef { contents = RuntimeObject.IntVector array' } -> accu32 array' array'.[index']
                | RuntimeRegister.RRef { contents = RuntimeObject.LongVector array' } -> accu64 array' array'.[index']
                | RuntimeRegister.RRef { contents = RuntimeObject.NativeIntVector array' } -> accun array' array'.[index']
                | RuntimeRegister.RRef { contents = RuntimeObject.StructVector array' } -> accstr array' array'.[index']
                | RuntimeRegister.RRef { contents = RuntimeObject.ObjectVector array' } -> accobj array' array'.[index']
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
                | Const_i32(value, Register dest) -> Const.i32 value dest
                | Const_true(Register dest) -> Const.``true`` dest
                | Const_false(Register dest)
                | Const_zero(Register dest) -> Const.``false`` dest
                | Ret(Registers frame' registers) ->
                    copyRegisterValues registers frame'.ReturnRegisters
                    frame <- frame'.Previous
                | Call(Method method, Registers frame' aregs, Registers frame' rregs) ->
                    invoke rregs aregs method
                | Call_ret(Method method, Registers frame' aregs, Registers frame' rregs) ->
                    // TODO: Test that tail calls work as intended.
                    frame <- frame'.Previous
                    invoke rregs aregs method
                    copyRegisterValues rregs frame'.ReturnRegisters
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
                | Obj_null(Register destination) ->
                    match destination with
                    | RuntimeRegister.RRef destination' -> destination'.contents <- RuntimeObject.Null
                    | RuntimeRegister.RNative destination' -> destination'.contents <- 0un
                    | _ -> raise(RuntimeException(frame, "Unable to store null reference into register"))
                | Obj_new(Method constructor, Registers frame' arguments, Register destination) ->
                    match destination with
                    | RuntimeRegister.RRef o ->
                        o.contents <- RuntimeObject.TypeInstance(constructor.DeclaringType, constructor.DeclaringType.InitializeObjectFields())
                        // TODO: Check that first argument is an object reference.
                        invoke ImmutableArray.Empty (arguments.Insert(0, destination)) constructor
                    | bad ->
                        failwithf "TODO: Error for cannot store object reference here after calling constructor %A" bad
                | Obj_ldfd(Field field, Register object, Register destination) ->
                    field.CheckMutate frame'

                    fieldAccessInstruction field object <| fun fields ->
                        match destination with // TODO: How to respect endianness when reading values from struct
                        | RuntimeRegister.R1 destination' ->
                            destination'.contents <- fields.ReadRaw<uint8> field.Offset
                        | RuntimeRegister.R2 destination' ->
                            destination'.contents <- fields.ReadRaw<uint16> field.Offset
                        | RuntimeRegister.R4 destination' ->
                            destination'.contents <- fields.ReadRaw<uint32> field.Offset
                        | RuntimeRegister.R8 destination' ->
                            destination'.contents <- fields.ReadRaw<uint64> field.Offset
                        | RuntimeRegister.RNative destination' ->
                            destination'.contents <- fields.ReadRaw<unativeint> field.Offset
                        | RuntimeRegister.RStruct _ ->
                            failwith "// TODO: How to read structs stored inside other structs?"
                        | RuntimeRegister.RRef destination' ->
                            destination'.contents <- fields.ReadRef(field.Offset)
                | Obj_stfd(Field field, Register object, Register source) ->
                    fieldAccessInstruction field object <| fun fields ->
                        match source with
                        | RuntimeRegister.R1 { contents = value } -> fields.WriteRaw(field.Offset, value)
                        | RuntimeRegister.R2 { contents = value } -> fields.WriteRaw(field.Offset, value)
                        | RuntimeRegister.R4 { contents = value } -> fields.WriteRaw(field.Offset, value)
                        | RuntimeRegister.R8 { contents = value } -> fields.WriteRaw(field.Offset, value)
                        | RuntimeRegister.RNative { contents = value } -> fields.WriteRaw(field.Offset, value)
                        | RuntimeRegister.RStruct _ ->
                            failwith "// TODO: How to store structs stored inside other structs?"
                        | RuntimeRegister.RRef { contents = value } -> fields.WriteRef(field.Offset, value)
                | Obj_arr_new(TypeSignature etype, Register length, Register destination) ->
                    let length' =
                        match length with // TODO: Really need to keep track if an integer register is signed or not to determine array length
                        | RuntimeRegister.R1 { contents = value } -> int32 value
                        | RuntimeRegister.R2 { contents = value } -> int32 value
                        | RuntimeRegister.R4 { contents = value } -> int32 value
                        | RuntimeRegister.R8 { contents = value } -> int32 value
                        | RuntimeRegister.RNative { contents = value } -> int32 value

                    match destination with
                    | RuntimeRegister.RRef destination' ->
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
                | Obj_arr_len(Register array, Register length) ->
                    // TODO: Make common function for assuming register contains object reference
                    match array with
                    | RuntimeRegister.RRef { contents = o } ->
                        let inline (|Length|) array = ((^T) : (member Length : int32) array)
                        match o with
                        | RuntimeObject.ByteVector(Length len)
                        | RuntimeObject.ShortVector(Length len)
                        | RuntimeObject.IntVector(Length len)
                        | RuntimeObject.LongVector(Length len)
                        | RuntimeObject.NativeIntVector(Length len)
                        | RuntimeObject.StructVector(Length len)
                        | RuntimeObject.ObjectVector(Length len) ->
                            Const.i32 len length
                        | _ -> failwith "TODO: Error for cannot get length of array when object reference is not to an array"
                    | _ -> failwith "TODO: Error for expected object reference to array but got value type"
                | Obj_arr_get(Register array, Register index, Register destination) ->
                    arrayAccessInstruction array index
                        (fun _ _ -> failwith "TODO: Array u8 element not supported")
                        (fun _ _ -> failwith "TODO: Array u16 element not supported")
                        (fun _ i -> Const.i32 (int32 i) destination) // TODO: Should store an unsigned integer instead.
                        (fun _ _ -> failwith "TODO: Array u64 element not supported")
                        (fun _ _ -> failwith "TODO: Array unative element not supported")
                        (fun _ _ -> failwith "TODO: Array struct element not supported")
                        (fun _ i -> RuntimeRegister.RRef(ref i).CopyValueTo(destination)) // TODO: Define a Const.obj function instead
                | Obj_arr_set(Register array, Register index, Register source) ->
                    failwith "TODO: Define helper functions for reading values registers"
                | Nop -> ()
                | bad -> failwithf "TODO: Unsupported instruction %A" bad

                frame'.InstructionIndex <- Checked.(+) frame'.InstructionIndex 1
            with
            | e ->
                //ex <- ValueSome e
                raise(System.NotImplementedException("TODO: Implement exception handling", e))

            ()

        match frame with
        | ValueNone -> ()
        | ValueSome _ -> raise(MissingReturnInstructionException(frame, "Reached unexpected end of instructions"))

// TODO: Make a RuntimeArray type.

[<Sealed>]
type InvalidConstructorException (method: RuntimeMethod, frame, message) =
    inherit RuntimeException(frame, message)

    member _.Method = method

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
            | ValueType.Primitive PrimitiveType.Bool
            | ValueType.Primitive PrimitiveType.S8
            | ValueType.Primitive PrimitiveType.U8 -> fun() -> RuntimeRegister.R1(ref 0uy)
            | ValueType.Primitive PrimitiveType.S16
            | ValueType.Primitive PrimitiveType.U16
            | ValueType.Primitive PrimitiveType.Char16 -> fun() -> RuntimeRegister.R2(ref 0us)
            | ValueType.Primitive PrimitiveType.S32
            | ValueType.Primitive PrimitiveType.U32
            | ValueType.Primitive PrimitiveType.F32
            | ValueType.Primitive PrimitiveType.Char32 -> fun() -> RuntimeRegister.R4(ref 0u)
            | ValueType.Primitive PrimitiveType.S64
            | ValueType.Primitive PrimitiveType.U64
            | ValueType.Primitive PrimitiveType.F64 -> fun() -> RuntimeRegister.R8(ref 0UL)
            | ValueType.Primitive PrimitiveType.SNative
            | ValueType.Primitive PrimitiveType.UNative
            | ValueType.UnsafePointer _ -> fun() -> RuntimeRegister.RNative(ref 0un)
            | ValueType.Primitive PrimitiveType.Unit -> fun() -> failwith "TODO: Prevent usage of Unit in register types."
            | ValueType.Defined _ -> failwith "TODO: Add support for registers containing structs"
        | ReferenceType _ -> fun() -> RuntimeRegister.RRef(ref RuntimeObject.Null)
        | SafePointer _ -> failwithf "TODO: Safe pointers in registers not yet supported"

    member this.CreateArgumentRegisters() =
        let { MethodSignature.ParameterTypes = atypes } = rmodule.MethodSignatureAt method.Signature
        let mutable registers = Array.zeroCreate atypes.Length

        for i = 0 to registers.Length - 1 do
            registers.[i] <- this.CreateRegister atypes.[i] ()

        Unsafe.As<RuntimeRegister[], ImmutableArray<RuntimeRegister>> &registers

    member this.SetupStackFrame(returns, frame: byref<_>) =
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

            frame <- ValueSome(RuntimeStackFrame(frame, args, registers, returns, code.Instructions, this))
        | MethodBody.Abstract -> failwith "TODO: Handle virtual calls"

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

    member _.Module = rm

    member _.Layout: RuntimeTypeLayout = layout.Value

    member val Name = rm.IdentifierAt t.TypeName

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

    member _.IdentifierAt(Index i: IdentifierIndex) = m.Identifiers.Identifiers.[Checked.int32 i]

    member this.NamespaceAt(Index i: NamespaceIndex): string =
        m.Namespaces.[Checked.int32 i] |> Seq.map this.IdentifierAt |> String.concat "." // TODO: Cache namespaces

    member _.TypeSignatureAt(Index i: TypeSignatureIndex) = m.TypeSignatures.[Checked.int32 i]

    member _.MethodSignatureAt(Index i: MethodSignatureIndex) = m.MethodSignatures.[Checked.int32 i]

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
                                |> RuntimeRegister.RRef
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

            let result = ref 0u // TODO: Check that entry point does not return more than 1 value.

            Interpreter.interpret (ImmutableArray.Create(RuntimeRegister.R4 result)) arguments main
            int32 result.contents
        | ValueNone -> raise(MissingEntryPointException(this, "The entry point method of the module is not defined"))

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
