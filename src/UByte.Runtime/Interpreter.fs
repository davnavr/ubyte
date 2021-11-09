﻿[<RequireQualifiedAccess>]
module UByte.Runtime.Interpreter

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open Microsoft.FSharp.NativeInterop

open UByte.Format.Model
open UByte.Format.Model.InstructionSet

open UByte.Resolver
open UByte.Runtime.MemoryManagement

#nowarn "9"

let inline isFlagSet flag value = value &&& flag = flag

[<RequireQualifiedAccess; IsReadOnly; Struct; NoComparison; NoEquality>]
type TypeLayout =
    { /// The size, in bytes, of instances of the type.
      Size: int32
      Fields: Dictionary<ResolvedField, int32>
      /// The offsets to the fields containing object references.
      References: ImmutableArray<int32> }

let private sizeOfType (md: ResolvedModule) (typeLayoutResolver: ResolvedTypeDefinition -> TypeLayout) ty =
    match ty with
    | AnyType.ValueType(ValueType.Primitive prim) ->
        match prim with
        | PrimitiveType.Bool | PrimitiveType.S8 | PrimitiveType.U8 -> 1
        | PrimitiveType.S16 | PrimitiveType.U16 | PrimitiveType.Char16 -> 2
        | PrimitiveType.S32 | PrimitiveType.U32 | PrimitiveType.Char32 | PrimitiveType.F32 -> 4
        | PrimitiveType.S64 | PrimitiveType.U64 | PrimitiveType.F64 -> 8
        | PrimitiveType.SNative | PrimitiveType.UNative -> sizeof<nativeint>
    | AnyType.ValueType(ValueType.Defined tindex) ->
        (md.TypeAt tindex |> typeLayoutResolver).Size
    | AnyType.ReferenceType _ -> sizeof<ObjectReference>
    | AnyType.ValueType(ValueType.UnsafePointer _) | AnyType.SafePointer _ -> sizeof<voidptr>

[<RequireQualifiedAccess>]
module private Array =
    let allocate (gc: IGarbageCollector) etype length =
        gc.Allocate(failwith "TODO: Get index for array type, not type of element", sizeof<int32> + (failwith "TODO: Get array element size" * length))

    let length array =
        ObjectReference.toVoidPtr array
        |> NativePtr.ofVoidPtr<int32>
        |> NativePtr.read

    let address (ObjectReference addr) =
        addr + nativeint sizeof<int32>
        |> NativePtr.ofNativeInt<byte>
        |> NativePtr.toVoidPtr

[<RequireQualifiedAccess; Struct; NoComparison; NoEquality>]
type Register =
    { // Assumes that all integer types can fit in 64-bits.
      mutable Value: uint64
      Type: RegisterType }

    override this.ToString() =
        match this.Type with
        | RegisterType.Primitive prim ->
            match prim with
            | PrimitiveType.Bool -> sprintf "%b" (this.Value = 0UL)
            | PrimitiveType.U8 -> sprintf "%iuy" (uint8 this.Value)
            | PrimitiveType.S8 -> sprintf "%iy" (int8 this.Value)
            | PrimitiveType.U16 -> sprintf "%ius" (uint16 this.Value)
            | PrimitiveType.S16 -> sprintf "%is" (int16 this.Value)
            | PrimitiveType.Char16 -> string(char this.Value)
            | PrimitiveType.U32 -> sprintf "%iu" (uint32 this.Value)
            | PrimitiveType.S32 -> string(int32 this.Value)
            | PrimitiveType.Char32 -> System.Text.Rune(int32 this.Value).ToString()
            | PrimitiveType.U64 -> sprintf "%iUL" this.Value
            | PrimitiveType.S64 -> sprintf "%iL" (int64 this.Value)
            | PrimitiveType.UNative -> sprintf "%iun" (unativeint this.Value)
            | PrimitiveType.SNative -> sprintf "%in" (nativeint this.Value)
            | PrimitiveType.F32 -> string(Unsafe.As<_, single> &this.Value)
            | PrimitiveType.F64 -> string(Unsafe.As<_, double> &this.Value)
        | RegisterType.Object | RegisterType.Pointer -> sprintf "0x%016X" this.Value

let private anyTypeToRegisterType ty =
    match ty with
    | AnyType.ValueType(ValueType.Primitive prim) ->
        RegisterType.Primitive prim
    | AnyType.ReferenceType _ ->
        RegisterType.Object
    | AnyType.SafePointer _ | AnyType.ValueType(ValueType.Defined _ | ValueType.UnsafePointer _) ->
        RegisterType.Pointer

[<RequireQualifiedAccess>]
module Register =
    let ofRegisterType rtype = { Register.Value = Unchecked.defaultof<uint64>; Register.Type = rtype }

    let ofTypeIndex (rm: ResolvedModule) typei =
        rm.TypeSignatureAt typei
        |> anyTypeToRegisterType
        |> ofRegisterType

    let ofValue<'Value when 'Value : unmanaged> rtype (value: 'Value) =
        let mutable register = ofRegisterType rtype
        Unsafe.As<uint64, 'Value> &register.Value <- value
        register

[<Sealed>]
type StackFrame
    (
        prev: StackFrame voption,
        args: Register[],
        localRegisterCount: uint32,
        returns: Register[],
        blocks: ImmutableArray<CodeBlock>,
        method: ResolvedMethod
    )
    =
    let mutable bindex, iindex = 0, 0
    let mutable previousBlockIndex = ValueNone
    let currentLocalLookup = Dictionary<LocalIndex, TemporaryIndex>(Checked.int32 localRegisterCount)
    let previousLocalLookup = Dictionary<LocalIndex, Register>(Checked.int32 localRegisterCount)
    do
        if not blocks.IsDefaultOrEmpty then
            // TODO: Avoid code duplication with JumpTo
            for struct(tindex, lindex) in blocks.[0].Locals do
                currentLocalLookup.Add(lindex, tindex)

    member _.ArgumentRegisters = args
    member _.InstructionIndex with get() = iindex and set i = iindex <- i
    member _.BlockIndex = bindex
    member _.PreviousBlockIndex = previousBlockIndex
    member val TemporaryRegisters = List<Register>()
    member _.ReturnRegisters = returns
    member _.Code = blocks
    member _.CurrentMethod = method
    member _.CurrentModule = method.DeclaringModule
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

/// Contains functions for retrieving values from registers.
[<RequireQualifiedAccess>]
module private InterpretRegister =
    let inline value<'Value when 'Value : unmanaged> (register: inref<Register>) =
        Unsafe.As<_, 'Value>(&Unsafe.AsRef(&register).Value)

/// Contains functions for converting values stored in registers.
[<RequireQualifiedAccess>]
module private ConvertRegister =
    let private noObjectReference() = invalidOp "Cannot convert object reference into a numeric value"

    let inline private number (register: Register) u8 s8 u16 s16 u32 s32 u64 s64 f32 f64 unative snative =
        match register.Type with
        | RegisterType.Primitive PrimitiveType.Bool
        | RegisterType.Primitive PrimitiveType.U8 -> u8(InterpretRegister.value<uint8> &register)
        | RegisterType.Primitive PrimitiveType.S8 -> s8(InterpretRegister.value<int8> &register)
        | RegisterType.Primitive PrimitiveType.Char16
        | RegisterType.Primitive PrimitiveType.U16 -> u16(InterpretRegister.value<uint16> &register)
        | RegisterType.Primitive PrimitiveType.S16 -> s16(InterpretRegister.value<int16> &register)
        | RegisterType.Primitive PrimitiveType.Char32
        | RegisterType.Primitive PrimitiveType.U32 -> u32(InterpretRegister.value<uint32> &register)
        | RegisterType.Primitive PrimitiveType.S32 -> s32(InterpretRegister.value<int32> &register)
        | RegisterType.Primitive PrimitiveType.U64 -> u64 register.Value
        | RegisterType.Primitive PrimitiveType.S64 -> s64(InterpretRegister.value<int64> &register)
        | RegisterType.Primitive PrimitiveType.F32 -> f32(InterpretRegister.value<single> &register)
        | RegisterType.Primitive PrimitiveType.F64 -> f64(InterpretRegister.value<double> &register)
        | RegisterType.Primitive PrimitiveType.UNative -> unative(InterpretRegister.value<unativeint> &register)
        | RegisterType.Primitive PrimitiveType.SNative | RegisterType.Pointer ->
            snative(InterpretRegister.value<nativeint> &register)
        | RegisterType.Object -> noObjectReference()

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
module private RegisterArithmetic =
    let private noObjectReferences() = invalidOp "Cannot use object reference in an arithmetic operation"

    let inline private binop opu8 ops8 opu16 ops16 opu32 ops32 opu64 ops64 opunative opsnative opf32 opf64 rtype xreg yreg =
        match rtype with
        | RegisterType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool) ->
            Register.ofValue rtype (opu8 (ConvertRegister.u8 xreg) (ConvertRegister.u8 yreg))
        | RegisterType.Primitive PrimitiveType.S8 ->
            Register.ofValue rtype (ops8 (ConvertRegister.s8 xreg) (ConvertRegister.s8 yreg))
        | RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16) ->
            Register.ofValue rtype (opu16 (ConvertRegister.u16 xreg) (ConvertRegister.u16 yreg))
        | RegisterType.Primitive PrimitiveType.S16 ->
            Register.ofValue rtype (ops16 (ConvertRegister.s16 xreg) (ConvertRegister.s16 yreg))
        | RegisterType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32) ->
            Register.ofValue rtype (opu32 (ConvertRegister.u32 xreg) (ConvertRegister.u32 yreg))
        | RegisterType.Primitive PrimitiveType.S32 ->
            Register.ofValue rtype (ops32 (ConvertRegister.s32 xreg) (ConvertRegister.s32 yreg))
        | RegisterType.Primitive PrimitiveType.U64 ->
            Register.ofValue rtype (opu64 (ConvertRegister.u64 xreg) (ConvertRegister.u64 yreg))
        | RegisterType.Primitive PrimitiveType.S64 ->
            Register.ofValue rtype (ops64 (ConvertRegister.s64 xreg) (ConvertRegister.s64 yreg))
        | RegisterType.Primitive PrimitiveType.UNative ->
            Register.ofValue rtype (opunative (ConvertRegister.unative xreg) (ConvertRegister.unative yreg))
        | RegisterType.Primitive PrimitiveType.SNative | RegisterType.Pointer ->
            Register.ofValue rtype (opsnative (ConvertRegister.snative xreg) (ConvertRegister.snative yreg))
        | RegisterType.Primitive PrimitiveType.F32 ->
            Register.ofValue rtype (opf32 (ConvertRegister.f32 xreg) (ConvertRegister.f32 yreg))
        | RegisterType.Primitive PrimitiveType.F64 ->
            Register.ofValue rtype (opf64 (ConvertRegister.f64 xreg) (ConvertRegister.f64 yreg))
        | RegisterType.Object -> noObjectReferences()

    let add rtype xreg yreg = binop (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) rtype xreg yreg
    let sub rtype xreg yreg = binop (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) rtype xreg yreg
    let mul rtype xreg yreg = binop (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) rtype xreg yreg

    let inline private integerDivideOperation op x y =
        if y <> LanguagePrimitives.GenericZero
        then op x y
        else raise(NotImplementedException "Integer division that cannot throw on division by zero is not implemented")

    let div rtype xreg yreg =
        binop (integerDivideOperation (/)) (integerDivideOperation (/)) (integerDivideOperation (/))
            (integerDivideOperation (/)) (integerDivideOperation (/)) (integerDivideOperation (/))
            (integerDivideOperation (/)) (integerDivideOperation (/)) (integerDivideOperation (/))
            (integerDivideOperation (/)) (/) (/) rtype xreg yreg

    let rem rtype xreg yreg =
        binop (integerDivideOperation (%)) (integerDivideOperation (%)) (integerDivideOperation (%))
            (integerDivideOperation (%)) (integerDivideOperation (%)) (integerDivideOperation (%))
            (integerDivideOperation (%)) (integerDivideOperation (%)) (integerDivideOperation (%))
            (integerDivideOperation (%)) (/) (/) rtype xreg yreg

    let inline private bbinop opu8 ops8 opu16 ops16 opu32 ops32 opu64 ops64 opunative opsnative rtype xreg yreg =
        match rtype with
        | RegisterType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool) ->
            Register.ofValue rtype (opu8 (ConvertRegister.u8 xreg) (ConvertRegister.u8 yreg))
        | RegisterType.Primitive PrimitiveType.S8 ->
            Register.ofValue rtype (ops8 (ConvertRegister.s8 xreg) (ConvertRegister.s8 yreg))
        | RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16) ->
            Register.ofValue rtype (opu16 (ConvertRegister.u16 xreg) (ConvertRegister.u16 yreg))
        | RegisterType.Primitive PrimitiveType.S16 ->
            Register.ofValue rtype (ops16 (ConvertRegister.s16 xreg) (ConvertRegister.s16 yreg))
        | RegisterType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32 | PrimitiveType.F32) ->
            Register.ofValue rtype (opu32 (ConvertRegister.u32 xreg) (ConvertRegister.u32 yreg))
        | RegisterType.Primitive PrimitiveType.S32 ->
            Register.ofValue rtype (ops32 (ConvertRegister.s32 xreg) (ConvertRegister.s32 yreg))
        | RegisterType.Primitive(PrimitiveType.U64 | PrimitiveType.F64) ->
            Register.ofValue rtype (opu64 (ConvertRegister.u64 xreg) (ConvertRegister.u64 yreg))
        | RegisterType.Primitive PrimitiveType.S64 ->
            Register.ofValue rtype (ops64 (ConvertRegister.s64 xreg) (ConvertRegister.s64 yreg))
        | RegisterType.Primitive PrimitiveType.UNative ->
            Register.ofValue rtype (opunative (ConvertRegister.unative xreg) (ConvertRegister.unative yreg))
        | RegisterType.Primitive PrimitiveType.SNative | RegisterType.Pointer ->
            Register.ofValue rtype (opsnative (ConvertRegister.snative xreg) (ConvertRegister.snative yreg))
        | RegisterType.Object -> noObjectReferences()

    let ``and`` vtype xreg yreg =
        bbinop (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (RegisterType.Primitive vtype) xreg yreg

    let ``or`` vtype xreg yreg =
        bbinop (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) (RegisterType.Primitive vtype) xreg yreg

    let xor vtype xreg yreg =
        bbinop (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (RegisterType.Primitive vtype) xreg yreg

    let inline private unop opu8 ops8 opu16 ops16 opu32 ops32 opu64 ops64 opunative opsnative opf32 opf64 rtype register =
        match rtype with
        | RegisterType.Pointer | RegisterType.Primitive PrimitiveType.SNative ->
            Register.ofValue rtype (opsnative(InterpretRegister.value<nativeint> &register))
        | RegisterType.Primitive(PrimitiveType.Bool | PrimitiveType.U8) ->
            Register.ofValue rtype (opu8(InterpretRegister.value<uint8> &register))
        | RegisterType.Primitive PrimitiveType.S8 ->
            Register.ofValue rtype (ops8(InterpretRegister.value<int8> &register))
        | RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16) ->
            Register.ofValue rtype (opu16(InterpretRegister.value<uint16> &register))
        | RegisterType.Primitive PrimitiveType.S16 ->
            Register.ofValue rtype (ops16(InterpretRegister.value<int16> &register))
        | RegisterType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32) ->
            Register.ofValue rtype (opu32(InterpretRegister.value<uint32> &register))
        | RegisterType.Primitive PrimitiveType.S32 ->
            Register.ofValue rtype (ops32(InterpretRegister.value<int32> &register))
        | RegisterType.Primitive PrimitiveType.U64 ->
            Register.ofValue rtype (opu64(InterpretRegister.value<uint64> &register))
        | RegisterType.Primitive PrimitiveType.S64 ->
            Register.ofValue rtype (ops64(InterpretRegister.value<int64> &register))
        | RegisterType.Primitive PrimitiveType.UNative ->
            Register.ofValue rtype (opunative(InterpretRegister.value<unativeint> &register))
        | RegisterType.Primitive PrimitiveType.F32 ->
            Register.ofValue rtype (opf32(InterpretRegister.value<single> &register))
        | RegisterType.Primitive PrimitiveType.F64 ->
            Register.ofValue rtype (opf64(InterpretRegister.value<double> &register))
        | RegisterType.Object -> noObjectReferences()

    let inline private oneop (op: _ -> _ -> _) = op LanguagePrimitives.GenericOne

    let inline private inc value = oneop (+) value
    let incr rtype register = unop inc inc inc inc inc inc inc inc inc inc inc inc rtype register
    let inline private dec value = oneop (-) value
    let decr rtype register = unop dec dec dec dec dec dec dec dec dec dec dec dec rtype register

    let inline private bunop opu8 ops8 opu16 ops16 opu32 ops32 opu64 ops64 opunative opsnative rtype register =
        match rtype with
        | RegisterType.Pointer | RegisterType.Primitive PrimitiveType.SNative ->
            Register.ofValue rtype (opsnative(InterpretRegister.value<nativeint> &register))
        | RegisterType.Primitive(PrimitiveType.Bool | PrimitiveType.U8) ->
            Register.ofValue rtype (opu8(InterpretRegister.value<uint8> &register))
        | RegisterType.Primitive PrimitiveType.S8 ->
            Register.ofValue rtype (ops8(InterpretRegister.value<int8> &register))
        | RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16) ->
            Register.ofValue rtype (opu16(InterpretRegister.value<uint16> &register))
        | RegisterType.Primitive PrimitiveType.S16 ->
            Register.ofValue rtype (ops16(InterpretRegister.value<int16> &register))
        | RegisterType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32 | PrimitiveType.F32) ->
            Register.ofValue rtype (opu32(InterpretRegister.value<uint32> &register))
        | RegisterType.Primitive PrimitiveType.S32 ->
            Register.ofValue rtype (ops32(InterpretRegister.value<int32> &register))
        | RegisterType.Primitive(PrimitiveType.U64 | PrimitiveType.F64) ->
            Register.ofValue rtype (opu64(InterpretRegister.value<uint64> &register))
        | RegisterType.Primitive PrimitiveType.S64 ->
            Register.ofValue rtype (ops64(InterpretRegister.value<int64> &register))
        | RegisterType.Primitive PrimitiveType.UNative ->
            Register.ofValue rtype (opunative(InterpretRegister.value<unativeint> &register))
        | RegisterType.Object -> noObjectReferences()

    let ``not`` rtype register =
        bunop (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (RegisterType.Primitive rtype) register

    [<RequireQualifiedAccess>]
    module Checked =
        open Microsoft.FSharp.Core.Operators.Checked

        let add vtype xreg yreg = binop (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) vtype xreg yreg
        let sub vtype xreg yreg = binop (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) vtype xreg yreg
        let mul vtype xreg yreg = binop (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) vtype xreg yreg

        let inline private floatDivOp op x y =
            if y <> LanguagePrimitives.GenericZero
            then op x y
            else raise(DivideByZeroException())

        let div vtype xreg yreg = binop (/) (/) (/) (/) (/) (/) (/) (/) (/) (/) (floatDivOp (/)) (floatDivOp (/)) vtype xreg yreg
        let rem vtype xreg yreg = binop (%) (%) (%) (%) (%) (%) (%) (%) (%) (%) (floatDivOp (%)) (floatDivOp (%)) vtype xreg yreg

        let inline private inc value = oneop (+) value
        let incr vtype register = unop inc inc inc inc inc inc inc inc inc inc inc inc vtype register
        let inline private dec value = oneop (-) value
        let decr vtype register = unop dec dec dec dec dec dec dec dec dec dec dec dec vtype register

/// Contains functions for comparing the values stored in registers.
[<RequireQualifiedAccess>]
module private RegisterComparison =
    let isTrueValue (register: Register) = register.Value <> LanguagePrimitives.GenericZero

    let inline private comparison s8 u8 s16 u16 s32 u32 s64 u64 snative unative f32 f64 (xreg: Register) (yreg: Register) =
        match xreg.Type, yreg.Type with
        | (RegisterType.Pointer | RegisterType.Object | RegisterType.Primitive PrimitiveType.SNative),
          (RegisterType.Pointer | RegisterType.Object | RegisterType.Primitive PrimitiveType.SNative) ->
            snative (InterpretRegister.value<nativeint> &xreg) (InterpretRegister.value<nativeint> &yreg)
        | (RegisterType.Object _, _) | (_, RegisterType.Object) ->
            invalidOp "Comparing an object reference and a numeric value is prohibited"
        | (RegisterType.Primitive PrimitiveType.SNative | RegisterType.Pointer), RegisterType.Primitive PrimitiveType.UNative ->
            let x = InterpretRegister.value<nativeint> &xreg
            if x < 0n
            then failwith "TODO: How to compare long and ulong?"
            else unative (unativeint x) (InterpretRegister.value<unativeint> &yreg)
        | RegisterType.Primitive PrimitiveType.UNative, (RegisterType.Primitive PrimitiveType.SNative | RegisterType.Pointer) ->
            let y = InterpretRegister.value<nativeint> &yreg
            if y < 0n
            then failwith "TODO: How to compare long and ulong?"
            else unative (InterpretRegister.value<unativeint> &xreg) (unativeint y)
        | _, RegisterType.Primitive PrimitiveType.F64 ->
            f64 (ConvertRegister.f64 xreg) (InterpretRegister.value<double> &yreg)
        | RegisterType.Primitive PrimitiveType.F64, _ ->
            f64 (InterpretRegister.value<double> &xreg) (ConvertRegister.f64 yreg)
        | RegisterType.Primitive PrimitiveType.F32, _ ->
            f32 (InterpretRegister.value<single> &xreg) (ConvertRegister.f32 yreg)
        | _, RegisterType.Primitive PrimitiveType.F32 ->
            f32 (ConvertRegister.f32 xreg) (InterpretRegister.value<single> &yreg)
        | RegisterType.Primitive PrimitiveType.S64, RegisterType.Primitive PrimitiveType.U64 ->
            let x = InterpretRegister.value<int64> &xreg
            if x < 0L
            then failwith "TODO: How to compare long and ulong?"
            else u64 (uint64 x) (InterpretRegister.value<uint64> &yreg)
        | RegisterType.Primitive PrimitiveType.U64, RegisterType.Primitive PrimitiveType.S64 ->
            let y = InterpretRegister.value<int64> &yreg
            if y < 0L
            then failwith "TODO: How to compare long and ulong?"
            else u64 (InterpretRegister.value<uint64> &xreg) (uint64 y)
        | RegisterType.Primitive PrimitiveType.U64, _ ->
            u64 (InterpretRegister.value<uint64> &xreg) (ConvertRegister.u64 yreg)
        | _, RegisterType.Primitive PrimitiveType.U64 ->
            u64 (ConvertRegister.u64 xreg) (InterpretRegister.value<uint64> &yreg)
        | RegisterType.Primitive PrimitiveType.S64, _ ->
            s64 (InterpretRegister.value<int64> &xreg) (ConvertRegister.s64 yreg)
        | _, RegisterType.Primitive PrimitiveType.S64 ->
            s64 (ConvertRegister.s64 xreg) (InterpretRegister.value<int64> &yreg)
        | RegisterType.Primitive PrimitiveType.UNative, _ ->
            unative (InterpretRegister.value<unativeint> &xreg) (ConvertRegister.unative yreg)
        | _, RegisterType.Primitive PrimitiveType.UNative ->
            unative (ConvertRegister.unative xreg) (InterpretRegister.value<unativeint> &yreg)
        | (RegisterType.Primitive PrimitiveType.SNative | RegisterType.Pointer), _ ->
            snative (InterpretRegister.value<nativeint> &xreg) (ConvertRegister.snative yreg)
        | _, (RegisterType.Primitive PrimitiveType.SNative | RegisterType.Pointer) ->
            snative (ConvertRegister.snative xreg) (InterpretRegister.value<nativeint> &yreg)
        | RegisterType.Primitive PrimitiveType.S32, RegisterType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32)
        | RegisterType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32), RegisterType.Primitive PrimitiveType.S32 ->
            s64 (ConvertRegister.s64 xreg) (ConvertRegister.s64 yreg)
        | RegisterType.Primitive PrimitiveType.U32, _ ->
            u32 (InterpretRegister.value<uint32> &xreg) (ConvertRegister.u32 yreg)
        | _, RegisterType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32) ->
            u32 (ConvertRegister.u32 xreg) (InterpretRegister.value<uint32> &yreg)
        | RegisterType.Primitive PrimitiveType.S32, _ ->
            s32 (InterpretRegister.value<int32> &xreg) (ConvertRegister.s32 yreg)
        | _, RegisterType.Primitive PrimitiveType.S32 ->
            s32 (ConvertRegister.s32 xreg) (InterpretRegister.value<int32> &yreg)
        | RegisterType.Primitive PrimitiveType.S16, RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16)
        | RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16), RegisterType.Primitive PrimitiveType.S16 ->
            s32 (ConvertRegister.s32 xreg) (ConvertRegister.s32 yreg)
        | RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16), _ ->
            u16 (InterpretRegister.value<uint16> &xreg) (ConvertRegister.u16 yreg)
        | _, RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16) ->
            u16 (ConvertRegister.u16 xreg) (InterpretRegister.value<uint16> &yreg)
        | RegisterType.Primitive PrimitiveType.S16, _ ->
            s16 (InterpretRegister.value<int16> &xreg) (ConvertRegister.s16 yreg)
        | _, RegisterType.Primitive PrimitiveType.S16 ->
            s16 (ConvertRegister.s16 xreg) (InterpretRegister.value<int16> &yreg)
        | RegisterType.Primitive PrimitiveType.S8, RegisterType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool)
        | RegisterType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool), RegisterType.Primitive PrimitiveType.S8 ->
            s16 (ConvertRegister.s16 xreg) (ConvertRegister.s16 yreg)
        | RegisterType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool), RegisterType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool) ->
            u8 (InterpretRegister.value<uint8> &xreg) (InterpretRegister.value<uint8> &yreg)
        | RegisterType.Primitive PrimitiveType.S8, RegisterType.Primitive PrimitiveType.S8 ->
            s8 (InterpretRegister.value<int8> &xreg) (InterpretRegister.value<int8> &yreg)

    let isLessThan xreg yreg = comparison (<) (<) (<) (<) (<) (<) (<) (<) (<) (<) (<) (<) xreg yreg
    let isGreaterThan xreg yreg = comparison (>) (>) (>) (>) (>) (>) (>) (>) (>) (>) (>) (>) xreg yreg
    let isEqual xreg yreg = comparison (=) (=) (=) (=) (=) (=) (=) (=) (=) (=) (=) (=) xreg yreg
    let isLessOrEqual xreg yreg = comparison (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) xreg yreg
    let isGreaterOrEqual xreg yreg = comparison (>=) (>=) (>=) (>=) (>=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) xreg yreg

[<RequireQualifiedAccess>]
module ExternalCode =
    [<Literal>]
    let private InternalCall = "runmdl"

    let private lookup = Dictionary<struct(string * string), StackFrame -> unit>()

    let private println (frame: StackFrame) =
        failwith "TODO: How to print some other thing"
        stdout.WriteLine()

    do lookup.[(InternalCall, "testhelperprintln")] <- println
    do lookup.[(InternalCall, "break")] <- fun _ -> System.Diagnostics.Debugger.Launch() |> ignore

    let call library name =
        match lookup.TryGetValue(struct(library, name)) with
        | true, call' -> call'
        | false, _ -> fun _ -> failwithf "TODO: Handle external calls to %s in %s" library name

let private setupStackFrame (method: ResolvedMethod) (frame: StackFrame voption ref) (runExternalCode: byref<_ voption>) =
    let inline createRegisterList (indices: ImmutableArray<_>) = Array.init indices.Length <| fun i ->
        Register.ofTypeIndex method.DeclaringModule indices.[i]

    let args = createRegisterList method.Signature.ParameterTypes
    let returns = createRegisterList method.Signature.ReturnTypes

    match method.Body with
    | MethodBody.Defined codei ->
        let code = method.DeclaringModule.CodeAt codei
        frame.contents <- ValueSome(StackFrame(frame.Value, args, code.LocalCount, returns, code.Blocks, method))
    | MethodBody.Abstract ->
        if not method.IsVirtual then failwith "TODO: Error for abstract method must be virtual"
        invalidOp(sprintf "Cannot directly call %O, use the call.virt instruction and related instructions instead" method)
    | MethodBody.External(library, efunction) ->
        let library' = method.DeclaringModule.IdentifierAt library
        let efunction' = method.DeclaringModule.IdentifierAt efunction
        let runtimeStackFrame = StackFrame(frame.Value, args, 0u, returns, ImmutableArray.Empty, method)
        frame.contents <- ValueSome runtimeStackFrame
        runExternalCode <- ValueSome(ExternalCode.call library' efunction')

[<RequireQualifiedAccess>]
module private ValidFlags =
    let (|Arithmetic|) (flags: ArithmeticFlags) =
        if flags &&& (~~~ArithmeticFlags.ValidMask) <> ArithmeticFlags.None then
            failwithf "TODO: Bad arithmetic flags %A" flags
        flags

    let (|Allocation|) (flags: AllocationFlags) =
        if flags &&& (~~~AllocationFlags.ValidMask) <> AllocationFlags.None then
            failwithf "TODO: Bad allocation flags %A" flags
        flags

    let (|MemoryAccess|) mask (flags: MemoryAccessFlags) =
        if flags &&& (~~~mask) <> MemoryAccessFlags.None then
            failwithf "TODO: Bad memory access flags %A" flags
        flags

[<RequireQualifiedAccess>]
module private StoreConstant =
    let integer itype value =
        let rtype = RegisterType.Primitive itype
        match itype with
        | PrimitiveType.Bool | PrimitiveType.U8 -> Register.ofValue rtype (uint8 value)
        | PrimitiveType.S8 -> Register.ofValue rtype (int8 value)
        | PrimitiveType.U16 | PrimitiveType.Char16 -> Register.ofValue rtype (uint16 value)
        | PrimitiveType.S16 -> Register.ofValue rtype (int16 value)
        | PrimitiveType.U32 | PrimitiveType.Char32 -> Register.ofValue rtype (uint32 value)
        | PrimitiveType.S32 -> Register.ofValue rtype (int32 value)
        | PrimitiveType.U64 -> Register.ofValue rtype (uint64 value)
        | PrimitiveType.S64 -> Register.ofValue rtype (int64 value)
        | PrimitiveType.UNative -> Register.ofValue rtype (unativeint value)
        | PrimitiveType.SNative -> Register.ofValue rtype (nativeint value)
        | PrimitiveType.F32 -> Register.ofValue rtype (single value)
        | PrimitiveType.F64 -> Register.ofValue rtype (double value)

    let nullObjectReference = Register.ofValue RegisterType.Object ObjectReference.Null

let private interpret
    (gc: IGarbageCollector)
    maxStackCapacity
    (arguments: ImmutableArray<Register>)
    (entrypoint: ResolvedMethod)
    =
    let mutable frame: StackFrame voption ref = ref ValueNone
    let mutable runExternalCode: (StackFrame -> unit) voption = ValueNone
    let mutable ex = ValueNone
    use stack = new ValueStack(maxStackCapacity)

    let invoke flags (arguments: ReadOnlyMemory<Register>) (method: ResolvedMethod) =
        if isFlagSet CallFlags.RequiresTailCallOptimization flags then
            raise(NotImplementedException "Tail call optimization is not yet supported")
        stack.SaveAllocations()
        setupStackFrame method frame &runExternalCode
        let current = frame.Value.Value
        let arguments' = current.ArgumentRegisters
        if arguments.Length < arguments'.Length then
            invalidOp(sprintf "Expected %i arguments but only %i were provided" arguments'.Length arguments.Length)
        // TODO: Copy only the Value part of the argument register.
        // TODO: Check that argument register types match.
        arguments.Span.CopyTo(Span arguments')
        current.ReturnRegisters

    let entryPointResults = invoke CallFlags.None (arguments.AsMemory()) entrypoint

#if DEBUG
    let cont() =
#else
    let inline cont() =
#endif
        match frame.Value with
        | ValueSome current ->
            current.BlockIndex < current.Code.Length && current.InstructionIndex < current.CurrentBlock.Instructions.Length
        | ValueNone -> false

    while cont() do
        match ex with
        | ValueSome e ->
            raise(NotImplementedException("TODO: Reimplement exception handling, with support for moving back up the call stack", e))
        | ValueNone -> ()

        let control = frame.Value.Value

#if DEBUG
        let (|Register|) rindex =
#else
        let inline (|Register|) rindex =
#endif
            control.RegisterAt rindex

        let (|LookupRegisterArray|) (indices: ImmutableArray<RegisterIndex>) =
            let registers = Array.zeroCreate indices.Length // TODO: Cache register lookup array.
            for i = 0 to registers.Length - 1 do registers.[i] <- control.RegisterAt indices.[i]
            registers

        let inline (|Method|) mindex: ResolvedMethod = control.CurrentModule.MethodAt mindex
        let inline (|Field|) findex: ResolvedField = control.CurrentModule.FieldAt findex
        let inline (|TypeSignature|) tindex: AnyType = control.CurrentModule.TypeSignatureAt tindex
        let inline (|RegisterType|) (TypeSignature rtype) = anyTypeToRegisterType rtype
        //let inline (|TypeLayout|) (t: RuntimeTypeDefinition) = t.Layout
        let inline (|Data|) dindex: ImmutableArray<_> = control.CurrentModule.DataAt dindex
        let inline (|BranchTarget|) (target: BlockOffset) = Checked.(+) control.BlockIndex target

#if DEBUG
        let branchToTarget (BranchTarget target) =
#else
        let inline branchToTarget (BranchTarget target) =
#endif
            control.JumpTo target

        //fieldAccessInstruction

        //arrayAccessInstruction

        try
            let instr = control.CurrentBlock.Instructions.[control.InstructionIndex]

            match instr with
            | Phi values -> // TODO: In format, reverse order of indices so BlockOffset is first to allow usage as key in dictionary.
                match control.PreviousBlockIndex with
                | ValueSome prev ->
                    // TODO: Use for loop to avoid extra allocations, maybe even require sorting in the binary format.
                    let struct(valuei, _) = Seq.find (fun struct(_, BranchTarget blocki) -> blocki = prev) values
                    control.TemporaryRegisters.Add(control.RegisterAt valuei) // Implicit copy
                | ValueNone ->
                    invalidOp "Usage of phi instruction is prohibited in the first block of a method"
            | Select(Register condition, Register vtrue, Register vfalse) ->
                // Implicit copy
                control.TemporaryRegisters.Add(if RegisterComparison.isTrueValue condition then vtrue else vfalse)
            | Add(ValidFlags.Arithmetic flags, RegisterType rtype, Register x, Register y) ->
                if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                then RegisterArithmetic.Checked.add rtype x y
                else RegisterArithmetic.add rtype x y
                |> control.TemporaryRegisters.Add
            | Sub(ValidFlags.Arithmetic flags, RegisterType rtype, Register x, Register y) ->
                if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                then RegisterArithmetic.Checked.sub rtype x y
                else RegisterArithmetic.sub rtype x y
                |> control.TemporaryRegisters.Add
            | Mul(ValidFlags.Arithmetic flags, RegisterType rtype, Register x, Register y) ->
                if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                then RegisterArithmetic.Checked.mul rtype x y
                else RegisterArithmetic.mul rtype x y
                |> control.TemporaryRegisters.Add
            | Div(ValidFlags.Arithmetic flags, RegisterType vtype, Register x, Register y) ->
                if isFlagSet ArithmeticFlags.ThrowOnDivideByZero flags
                then RegisterArithmetic.Checked.div vtype x y
                else RegisterArithmetic.div vtype x y
                |> control.TemporaryRegisters.Add
            | Rem(ValidFlags.Arithmetic flags, RegisterType vtype, Register x, Register y) ->
                if isFlagSet ArithmeticFlags.ThrowOnDivideByZero flags
                then RegisterArithmetic.Checked.rem vtype x y
                else RegisterArithmetic.rem vtype x y
                |> control.TemporaryRegisters.Add
            | Incr(ValidFlags.Arithmetic flags, RegisterType vtype, Register register) ->
                if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                then RegisterArithmetic.Checked.incr vtype register
                else RegisterArithmetic.incr vtype register
                |> control.TemporaryRegisters.Add
            | Decr(ValidFlags.Arithmetic flags, RegisterType vtype, Register register) ->
                if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                then RegisterArithmetic.Checked.decr vtype register
                else RegisterArithmetic.decr vtype register
                |> control.TemporaryRegisters.Add
            // TODO: Update type annotation for bitwise instructions to be a full TypeSignatureIndex to allow usage of pointer types.
            | And(vtype, Register x, Register y) ->
                control.TemporaryRegisters.Add(RegisterArithmetic.``and`` vtype x y)
            | Or(vtype, Register x, Register y) ->
                control.TemporaryRegisters.Add(RegisterArithmetic.``or`` vtype x y)
            | Xor(vtype, Register x, Register y) ->
                control.TemporaryRegisters.Add(RegisterArithmetic.xor vtype x y)
            | Not(vtype, Register register) ->
                control.TemporaryRegisters.Add(RegisterArithmetic.``not`` vtype register)
            | Const_i(vtype, value) ->
                control.TemporaryRegisters.Add(StoreConstant.integer vtype value)
            | Const_true vtype ->
                control.TemporaryRegisters.Add(StoreConstant.integer vtype 1)
            | Const_false vtype | Const_zero vtype ->
                control.TemporaryRegisters.Add(StoreConstant.integer vtype 0)
            | Obj_null ->
                control.TemporaryRegisters.Add StoreConstant.nullObjectReference
            | Const_f32 _
            | Const_f64 _ ->
                raise(NotImplementedException "TODO: Storing of constant floating point integers is not yet supported")

            | Call(flags, Method method, LookupRegisterArray arguments) ->
                invoke flags (ReadOnlyMemory arguments) method |> ignore

            | Ret(LookupRegisterArray results) ->
                if results.Length < control.ReturnRegisters.Length then
                    sprintf "Expected to return %i values but only returned %i values"
                        control.ReturnRegisters.Length
                        results.Length
                    |> invalidOp

                Span(results).CopyTo(Span control.ReturnRegisters) // TODO: Maybe Register could be a reference type? Ensuring registers are copied correctly may be confusing.
                frame.Value <- control.Previous

                match control.Previous with
                | ValueSome caller -> caller.TemporaryRegisters.AddRange results // TODO: Maybe Register could be a reference type? Ensuring registers are copied correctly may be confusing.
                | ValueNone -> ()

                stack.FreeAllocations()
            | Br target -> branchToTarget target
            | Br_eq(Register x, Register y, ttrue, tfalse)
            | Br_ne(Register x, Register y, tfalse, ttrue) ->
                branchToTarget (if RegisterComparison.isEqual x y then ttrue else tfalse)
            | Br_lt(Register x, Register y, tfalse, ttrue) ->
                branchToTarget (if RegisterComparison.isLessThan x y then ttrue else tfalse)
            | Br_gt(Register x, Register y, tfalse, ttrue) ->
                branchToTarget (if RegisterComparison.isGreaterThan x y then ttrue else tfalse)
            | Br_le(Register x, Register y, tfalse, ttrue) ->
                branchToTarget (if RegisterComparison.isLessOrEqual x y then ttrue else tfalse)
            | Br_ge(Register x, Register y, tfalse, ttrue) ->
                branchToTarget (if RegisterComparison.isGreaterOrEqual x y then ttrue else tfalse)
            | Br_true(Register condition, ttrue, tfalse) ->
                branchToTarget (if RegisterComparison.isTrueValue condition then ttrue else tfalse)

            | Nop -> ()

            match runExternalCode with
            | ValueNone -> ()
            | ValueSome run ->
                let runtimeStackFrame = frame.Value.Value
                try
                    run runtimeStackFrame
                    // TODO: Avoid code duplication with ret.
                    frame.Value <- runtimeStackFrame.Previous
                    stack.FreeAllocations()
                finally
                    runExternalCode <- ValueNone

            // Incrementing here means index points to instruction that caused the exception in the stack frame.
            control.InstructionIndex <- Checked.(+) control.InstructionIndex 1
        with
        | e -> ex <- ValueSome e

    match ex with
    | ValueSome e -> raise e
    | ValueNone -> ()

    match frame.contents with
    | ValueNone -> entryPointResults
    | ValueSome remaining ->
        invalidOp(sprintf "Reached unexpected end of code in block %i" remaining.BlockIndex)

[<Sealed>]
type MissingEntryPointException (message: string) = inherit Exception(message)

[<Sealed>]
type ModuleNotFoundException (name: ModuleIdentifier, message) =
    inherit Exception(message)

    member _.Name = name

let moduleImportResolver (loader: ModuleIdentifier -> Module voption) =
    let resolver = ref Unchecked.defaultof<_>
    resolver.Value <- fun id ->
        match loader id with
        | ValueSome import -> ResolvedModule(import, resolver.Value)
        | ValueNone -> raise(ModuleNotFoundException(id, "Unable to find module " + string id))
    resolver.Value

let typeLayoutResolver() =
    let lookup = Dictionary<ResolvedTypeDefinition, TypeLayout>()
    let rec inner ty =
        match lookup.TryGetValue ty with
        | true, existing -> existing
        | false, _ ->
            // TODO: How to deal with the "Deadly Diamond of Death"
            // TODO: Update format to allow specifying of HOW fields are inherited (e.g. a flag before each type index).
            // TODO: In the future, check for recursion by ensuring that the list does not contain the current type
            let mutable size = 0
            let fields = Dictionary()
            let references = ImmutableArray.CreateBuilder()

            for baseType in ty.BaseTypes do
                let inheritedLayout = inner baseType // DDD
                for KeyValue(field, i) in inheritedLayout.Fields do
                    fields.Add(field, size + i)
                for offset in inheritedLayout.References do
                    references.Add(size + offset)
                size <- size + inheritedLayout.Size

            for defined in ty.DefinedFields do
                let fsize = sizeOfType ty.DeclaringModule inner defined.FieldType

                match defined.FieldType with
                | AnyType.ReferenceType _ -> references.Add size
                | _ -> ()

                size <- size + fsize

            { TypeLayout.Size = size
              Fields = fields
              References = references.ToImmutable() }
    inner

let [<Literal>] DefaultStackCapacity = 0xFFFFF

[<Sealed>]
type Runtime
    (
        program: Module,
        moduleImportLoader,
        garbageCollectorStrategy: IGarbageCollector
    )
    =
    let program = ResolvedModule(program, moduleImportResolver moduleImportLoader)
    let types = Dictionary<ObjectType, struct(ResolvedModule * AnyType)>()
    let layouts = typeLayoutResolver()

    static member Initialize
        (
            program,
            ?moduleImportLoader,
            ?garbageCollectorStrategy
        )
        =
        Runtime (
            program,
            defaultArg moduleImportLoader (fun _ -> ValueNone),
            defaultArg garbageCollectorStrategy (CollectionStrategies.NaiveMarkAndSweep())
        )

    member _.Program = program

    member _.InvokeEntryPoint(argv: string[], ?maxStackCapacity) =
        match program.EntryPoint with
        | ValueSome main ->
            if main.DeclaringModule <> program then
                raise(MissingEntryPointException "The entry point method for a module must be defined in the module")

            let arguments =
                if main.Signature.ParameterTypes.Length = 0 then
                    ImmutableArray.Empty
                else
                    failwith "TODO: argv is not yet supported"

            if main.Signature.ReturnTypes.Length > 1 then
                failwith "TODO: Error for multiple return values are not supported in entry point"

            let results = interpret garbageCollectorStrategy (defaultArg maxStackCapacity DefaultStackCapacity) arguments main

            if Array.isEmpty results then 0 else int32 results.[0].Value
        | ValueNone ->
            raise(MissingEntryPointException "The entry point method of the module is not defined")
