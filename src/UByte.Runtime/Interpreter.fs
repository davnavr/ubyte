[<RequireQualifiedAccess>]
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

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type TypeLayout =
    { /// The size, in bytes, of instances of the type.
      Size: int32
      Fields: Dictionary<ResolvedField, int32>
      /// The offsets to the fields containing object references.
      References: ImmutableArray<int32> }

type TypeSizeResolver = ResolvedModule -> AnyType -> int32
type ObjectTypeResolver = ResolvedModule -> AnyType -> ObjectType
type ObjectTypeLookup = ObjectType -> struct(ResolvedModule * AnyType)
type TypeLayoutResolver = ResolvedTypeDefinition -> TypeLayout

let private refOrValTypeToAnyType ty =
    match ty with
    | ReferenceOrValueType.Reference t -> AnyType.ReferenceType t
    | ReferenceOrValueType.Value t -> AnyType.ValueType t

let private anyTypeToRefOrValType ty =
    match ty with
    | AnyType.ReferenceType t ->  ReferenceOrValueType.Reference t
    | AnyType.ValueType t -> ReferenceOrValueType.Value t
    | AnyType.SafePointer _ -> invalidOp "Unexpected pointer type"

let private anyTypeToRegisterType (typeSizeResolver: TypeSizeResolver) rm ty =
    match ty with
    | AnyType.ValueType(ValueType.Primitive prim) ->
        RegisterType.Primitive prim
    | AnyType.ReferenceType _ ->
        RegisterType.Object
    | AnyType.SafePointer(ReferenceOrValueType.Reference _) ->
        RegisterType.pointer(uint32 sizeof<ObjectReference>)
    | AnyType.SafePointer(ReferenceOrValueType.Value vtype)
    | AnyType.ValueType(ValueType.UnsafePointer vtype | (ValueType.Defined _ as vtype)) ->
        AnyType.ValueType vtype
        |> typeSizeResolver rm
        |> uint32
        |> RegisterType.pointer

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type RegisterValue =
    val private largest : uint64 // Type of the largest value a register is expected to hold.

[<RequireQualifiedAccess; Struct; NoComparison; NoEquality>]
type Register =
    { mutable Value: RegisterValue
      Type: RegisterType }

    override this.ToString() =
        match this.Type with
        | RegisterType.Primitive prim ->
            match prim with
            | PrimitiveType.Bool -> if Unsafe.As<_, bool> &this.Value then "true" else "false"
            | PrimitiveType.U8 -> sprintf "%iuy" (Unsafe.As<_, uint8> &this.Value)
            | PrimitiveType.S8 -> sprintf "%iy" (Unsafe.As<_, int8> &this.Value)
            | PrimitiveType.U16 -> sprintf "%ius" (Unsafe.As<_, uint16> &this.Value)
            | PrimitiveType.S16 -> sprintf "%is" (Unsafe.As<_, int16> &this.Value)
            | PrimitiveType.Char16 -> string(Unsafe.As<_, char> &this.Value)
            | PrimitiveType.U32 -> sprintf "%iu" (Unsafe.As<_, uint32> &this.Value)
            | PrimitiveType.S32 -> string(Unsafe.As<_, int32> &this.Value)
            | PrimitiveType.Char32 -> (Unsafe.As<_, System.Text.Rune> &this.Value).ToString()
            | PrimitiveType.U64 -> sprintf "%iUL" (Unsafe.As<_, uint64> &this.Value)
            | PrimitiveType.S64 -> sprintf "%iL" (Unsafe.As<_, int64> &this.Value)
            | PrimitiveType.UNative -> sprintf "%iun" (Unsafe.As<_, unativeint> &this.Value)
            | PrimitiveType.SNative -> sprintf "%in" (Unsafe.As<_, nativeint> &this.Value)
            | PrimitiveType.F32 -> string(Unsafe.As<_, single> &this.Value)
            | PrimitiveType.F64 -> string(Unsafe.As<_, double> &this.Value)
        | RegisterType.Object | RegisterType.Pointer _ -> sprintf "0x%016X" (Unsafe.As<_, nativeint> &this.Value)

[<Interface>]
type IRegisterList =
    abstract RegisterAt : index: int32 -> inref<Register>

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type AllocatedRegister =
    val private registers: IRegisterList
    val private index: int32

    new (registers: Register[], index) =
        { registers = { new IRegisterList with member _.RegisterAt i = &registers.[i] }; index = index }

    new (registers: ImmutableArray<Register>, index) =
        { registers = { new IRegisterList with member _.RegisterAt i = &registers.ItemRef(i) }; index = index }

    new (registers: ImmutableArray<Register>.Builder, index) =
        { registers = { new IRegisterList with member _.RegisterAt i = &registers.ItemRef(i) }; index = index }

    member this.Register : inref<Register> = &this.registers.RegisterAt(this.index)

    override this.ToString() = this.Register.ToString()

[<RequireQualifiedAccess>]
module Register =
    let ofRegisterType rtype = { Register.Value = RegisterValue(); Register.Type = rtype }

    let ofTypeIndex typeSizeResolver (rm: ResolvedModule) typei =
        rm.TypeSignatureAt typei
        |> anyTypeToRegisterType typeSizeResolver rm
        |> ofRegisterType

    let ofValue<'Value when 'Value : unmanaged> rtype (value: 'Value) =
        let mutable register = ofRegisterType rtype
        Unsafe.As<RegisterValue, 'Value> &register.Value <- value
        register

    let ofValueAddress<'Value when 'Value : unmanaged> rtype address =
        NativePtr.ofVoidPtr address
        |> NativePtr.read<'Value>
        |> ofValue<'Value> rtype

[<RequireQualifiedAccess>]
module private StoreConstant =
    let inline private primitive u8 s8 u16 s16 u32 s32 u64 s64 unative snative f32 f64 itype value =
        let rtype = RegisterType.Primitive itype
        match itype with
        | PrimitiveType.Bool | PrimitiveType.U8 -> Register.ofValue rtype (u8 value)
        | PrimitiveType.S8 -> Register.ofValue rtype (s8 value)
        | PrimitiveType.U16 | PrimitiveType.Char16 -> Register.ofValue rtype (u16 value)
        | PrimitiveType.S16 -> Register.ofValue rtype (s16 value)
        | PrimitiveType.U32 | PrimitiveType.Char32 -> Register.ofValue rtype (u32 value)
        | PrimitiveType.S32 -> Register.ofValue rtype (s32 value)
        | PrimitiveType.U64 -> Register.ofValue rtype (u64 value)
        | PrimitiveType.S64 -> Register.ofValue rtype (s64 value)
        | PrimitiveType.UNative -> Register.ofValue rtype (unative value)
        | PrimitiveType.SNative -> Register.ofValue rtype (snative value)
        | PrimitiveType.F32 -> Register.ofValue rtype (f32 value)
        | PrimitiveType.F64 -> Register.ofValue rtype (f64 value)

    let integer itype value =
        primitive uint8 int8 uint16 int16 uint32 int32 uint64 int64 unativeint nativeint single double itype value

    let nullObjectReference = Register.ofValue RegisterType.Object ObjectReference.Null

    let fromVoidPtr ty address =
        match ty with
        | RegisterType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool) -> Register.ofValueAddress<uint8> ty address
        | RegisterType.Primitive PrimitiveType.S8 -> Register.ofValueAddress<int8> ty address
        | RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16) -> Register.ofValueAddress<uint16> ty address
        | RegisterType.Primitive PrimitiveType.S16 -> Register.ofValueAddress<int16> ty address
        | RegisterType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32) -> Register.ofValueAddress<uint32> ty address
        | RegisterType.Primitive PrimitiveType.S32 -> Register.ofValueAddress<int32> ty address
        | RegisterType.Primitive PrimitiveType.U64 -> Register.ofValueAddress<uint64> ty address
        | RegisterType.Primitive PrimitiveType.S64 -> Register.ofValueAddress<int64> ty address
        | RegisterType.Primitive PrimitiveType.F32 -> Register.ofValueAddress<single> ty address
        | RegisterType.Primitive PrimitiveType.F64 -> Register.ofValueAddress<double> ty address
        | RegisterType.Primitive PrimitiveType.SNative -> Register.ofValueAddress<nativeint> ty address
        | RegisterType.Object -> Register.ofValueAddress<ObjectReference> ty address
        | RegisterType.Pointer _ | RegisterType.Primitive PrimitiveType.UNative -> Register.ofValueAddress<unativeint> ty address

    [<RequireQualifiedAccess>]
    module Checked =
        open Microsoft.FSharp.Core.Operators.Checked

        let integer itype value =
            primitive uint8 int8 uint16 int16 uint32 int32 uint64 int64 unativeint nativeint single double itype value

/// Contains functions for retrieving values from registers.
[<RequireQualifiedAccess>]
module private InterpretRegister =
    let inline value<'Value when 'Value : unmanaged> (register: inref<Register>) =
        Unsafe.As<_, 'Value>(&Unsafe.AsRef(&register).Value)

    let copyValueTo<'Value when 'Value : unmanaged> (destination: voidptr) (register: inref<Register>) =
        NativePtr.write (NativePtr.ofVoidPtr<'Value> destination) (value<'Value> &register)

/// Contains functions for interacting with object references to arrays.
[<RequireQualifiedAccess>]
module private ArrayObject =
    type ArrayLength = int32

    /// Returns a (managed/safe) pointer to the first element of the array.
    let address array =
        let addr = ObjectReference.toNativePtr<byte> array
        NativePtr.toVoidPtr(NativePtr.add addr sizeof<ArrayLength>)

    let allocate
        rm
        (gc: IGarbageCollector)
        garbageCollectionState
        (typeSizeResolver: TypeSizeResolver)
        (arrayTypeResolver: ObjectTypeResolver)
        etype
        length
        =
        if length < 0 then invalidArg (nameof length) "Cannot allocate an array with a negative length"
        let arrayt = arrayTypeResolver rm (AnyType.ReferenceType(ReferenceType.Vector etype))
        let esize = typeSizeResolver rm (refOrValTypeToAnyType etype) * length
        let array = gc.Allocate(garbageCollectionState, arrayt, sizeof<ArrayLength> + esize)
        NativePtr.write (ObjectReference.toNativePtr<ArrayLength> array) length
        Span<byte>(address array, esize).Clear()
        array

    let length array =
        ObjectReference.toVoidPtr array
        |> NativePtr.ofVoidPtr<ArrayLength>
        |> NativePtr.read

    let getElementType (gc: IGarbageCollector) (objectTypeLookup: ObjectTypeLookup) array =
        let struct(md, arrayt) = objectTypeLookup(gc.TypeOf array)
        match arrayt with
        | AnyType.ReferenceType(ReferenceType.Vector etype) -> struct(md, refOrValTypeToAnyType etype)
        | bad -> invalidArg (nameof array) (sprintf "Expected an array reference, but got %A" bad)

    // NOTE: This function may be inefficient in loops, as the size of each element is looked up each time.
    let item gc objectTypeLookup (typeSizeResolver: TypeSizeResolver) array index (etype: outref<_>) =
        let struct(md, ty) as elemt = getElementType gc objectTypeLookup array
        let size = typeSizeResolver md ty
        let addr = NativePtr.ofVoidPtr<byte>(address array)
        etype <- elemt
        NativePtr.add addr (index * size) |> NativePtr.toVoidPtr

    let get gc objectTypeLookup typeSizeResolver array index =
        let mutable etype = Unchecked.defaultof<_>
        let address = item gc objectTypeLookup typeSizeResolver array index &etype
        let rtype =
            match getElementType gc objectTypeLookup array with
            | _, AnyType.ValueType(ValueType.Defined _) ->
                raise(NotImplementedException "TODO: Retrieval of structs from arrays is not yet supported")
            | rm, ty -> anyTypeToRegisterType typeSizeResolver rm ty

        StoreConstant.fromVoidPtr rtype address

    let set gc objectTypeLookup typeSizeResolver array (source: inref<Register>) index =
        let mutable etype = Unchecked.defaultof<_>
        let address = item gc objectTypeLookup typeSizeResolver array index &etype
        let rtype =
            match getElementType gc objectTypeLookup array with
            | _, AnyType.ValueType(ValueType.Defined _) ->
                raise(NotImplementedException "TODO: Storing of structs into arrays is not yet supported")
            | rm, ty -> anyTypeToRegisterType typeSizeResolver rm ty

        match rtype with
        | RegisterType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool) ->
            InterpretRegister.copyValueTo<uint8> address &source
        | RegisterType.Primitive PrimitiveType.S8 ->
            InterpretRegister.copyValueTo<int8> address &source
        | RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16) ->
            InterpretRegister.copyValueTo<uint16> address &source
        | RegisterType.Primitive PrimitiveType.S16 ->
            InterpretRegister.copyValueTo<int16> address &source
        | RegisterType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32) ->
            InterpretRegister.copyValueTo<int32> address &source
        | RegisterType.Primitive PrimitiveType.S32 ->
            InterpretRegister.copyValueTo<uint32> address &source
        | RegisterType.Primitive PrimitiveType.U64 ->
            InterpretRegister.copyValueTo<uint64> address &source
        | RegisterType.Primitive PrimitiveType.S64 ->
            InterpretRegister.copyValueTo<int64> address &source
        | RegisterType.Primitive PrimitiveType.F32 ->
            InterpretRegister.copyValueTo<single> address &source
        | RegisterType.Primitive PrimitiveType.F64 ->
            InterpretRegister.copyValueTo<double> address &source
        | RegisterType.Pointer _ | RegisterType.Primitive PrimitiveType.UNative ->
            InterpretRegister.copyValueTo<unativeint> address &source
        | RegisterType.Primitive PrimitiveType.SNative ->
            InterpretRegister.copyValueTo<nativeint> address &source
        | RegisterType.Object ->
            InterpretRegister.copyValueTo<ObjectReference> address &source

[<RequireQualifiedAccess>]
module TypeLayout =
    let ofAnyType (typeLayoutResolver: TypeLayoutResolver) (rm: ResolvedModule) ty =
        match ty with
        | AnyType.ReferenceType(ReferenceType.Defined tdef)
        | AnyType.ValueType(ValueType.Defined tdef | ValueType.UnsafePointer(ValueType.Defined tdef))
        | AnyType.SafePointer(ReferenceOrValueType.Reference(ReferenceType.Defined tdef))
        | AnyType.SafePointer(ReferenceOrValueType.Value(ValueType.Defined tdef | ValueType.UnsafePointer(ValueType.Defined tdef))) ->
            typeLayoutResolver(rm.TypeAt tdef)
        | _ -> invalidOp(sprintf "Cannot retrieve type layout for type %A" ty)

    let ofObjectReference (gc: IGarbageCollector) (objectTypeLookup: ObjectTypeLookup) typeLayoutResolver o =
        let struct(rm, ty) = objectTypeLookup(gc.TypeOf o)
        ofAnyType typeLayoutResolver rm ty

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
    let previousLocalLookup = Dictionary<LocalIndex, AllocatedRegister>(Checked.int32 localRegisterCount)
    do
        if not blocks.IsDefaultOrEmpty then
            // TODO: Avoid code duplication with JumpTo
            for struct(tindex, lindex) in blocks.[0].Locals do
                currentLocalLookup.Add(lindex, tindex)

    member _.ArgumentRegisters = args
    member _.InstructionIndex with get() = iindex and set i = iindex <- i
    member _.BlockIndex = bindex
    member _.PreviousBlockIndex = previousBlockIndex
    member _.PreviousLocalRegisters = previousLocalLookup
    member val TemporaryRegisters = ImmutableArray.CreateBuilder<Register>()
    member _.ReturnRegisters = returns
    member _.Code = blocks
    member _.CurrentMethod = method
    member _.CurrentModule = method.DeclaringModule
    member this.CurrentBlock = blocks.[this.BlockIndex]
    member _.Previous = prev
    member _.PreviousMethod =
        match prev with
        | ValueSome previous -> ValueSome previous.CurrentMethod
        | ValueNone -> ValueNone

    member this.CurrentExceptionHandler = this.CurrentBlock.ExceptionHandler

    member this.RegisterAt(RegisterIndex.Index index) =
        let tcount = uint32 this.TemporaryRegisters.Count
        if index < tcount then
            &this.TemporaryRegisters.ItemRef(Checked.int32 index)
        else
            let index' = Checked.(-) index tcount
            let acount = uint32 this.ArgumentRegisters.Length
            if index' < acount then
                &this.ArgumentRegisters.[Checked.int32 index']
            else
                let index' = LocalIndex.Index(index - tcount - acount)
                match currentLocalLookup.TryGetValue index' with
                | true, Index i -> &this.TemporaryRegisters.ItemRef(Checked.int32 i)
                | false, _ ->
                    let previousLocalFound, previousLocalRegister = previousLocalLookup.TryGetValue index'

                    if not previousLocalFound then
                        sprintf "A register corresponding to the index 0x%0X could not be found" index
                        |> KeyNotFoundException
                        |> raise

                    &previousLocalRegister.Register

    member this.JumpTo index =
        previousBlockIndex <- ValueSome bindex
        bindex <- index
        iindex <- -1

        let previousTemporaryRegisters = this.TemporaryRegisters.ToImmutable()
        for KeyValue(lindex, Index lregister) in currentLocalLookup do
            previousLocalLookup.[lindex] <- AllocatedRegister(previousTemporaryRegisters, Checked.int32 lregister)

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

/// Contains functions for converting values stored in registers.
[<RequireQualifiedAccess>]
module private ConvertRegister =
    let private noObjectReference() = invalidOp "Cannot convert object reference into a numeric value"

    let inline private number (register: inref<Register>) u8 s8 u16 s16 u32 s32 u64 s64 f32 f64 unative snative =
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
        | RegisterType.Primitive PrimitiveType.U64 -> u64(InterpretRegister.value<uint64> &register)
        | RegisterType.Primitive PrimitiveType.S64 -> s64(InterpretRegister.value<int64> &register)
        | RegisterType.Primitive PrimitiveType.F32 -> f32(InterpretRegister.value<single> &register)
        | RegisterType.Primitive PrimitiveType.F64 -> f64(InterpretRegister.value<double> &register)
        | RegisterType.Primitive PrimitiveType.UNative | RegisterType.Pointer _ ->
            unative(InterpretRegister.value<unativeint> &register)
        | RegisterType.Primitive PrimitiveType.SNative-> snative(InterpretRegister.value<nativeint> &register)
        | RegisterType.Object -> noObjectReference()

    let u8 (register: inref<_>) =
        number &register id uint8 uint8 uint8 uint8 uint8 uint8 uint8 uint8 uint8 uint8 uint8

    let s8 (register: inref<_>) =
        number &register int8 id int8 int8 int8 int8 int8 int8 int8 int8 int8 int8

    let u16 (register: inref<_>) =
        number &register uint16 uint16 id uint16 uint16 uint16 uint16 uint16 uint16 uint16 uint16 uint16

    let s16 (register: inref<_>) =
        number &register int16 int16 int16 id int16 int16 int16 int16 int16 int16 int16 int16

    let u32 (register: inref<_>) =
        number &register uint32 uint32 uint32 uint32 id uint32 uint32 uint32 uint32 uint32 uint32 uint32

    let s32 (register: inref<_>) = 
        number &register int32 int32 int32 int32 int32 id int32 int32 int32 int32 int32 int32

    let u64 (register: inref<_>) =
        number &register uint64 uint64 uint64 uint64 uint64 uint64 id uint64 uint64 uint64 uint64 uint64

    let s64 (register: inref<_>) =
        number &register int64 int64 int64 int64 int64 int64 int64 id int64 int64 int64 int64

    let f32 (register: inref<_>) =
        number &register float32 float32 float32 float32 float32 float32 float32 float32 id float32 float32 float32

    let f64 (register: inref<_>) =
        number &register float float float float float float float float float id float float

    let snative (register: inref<_>) =
        number &register nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint id

    let unative (register: inref<_>) =
        number &register unativeint unativeint unativeint unativeint unativeint  unativeint  unativeint  unativeint unativeint unativeint id unativeint

/// Contains functions for performing arithmetic on the values stored in registers.
[<RequireQualifiedAccess>]
module private RegisterArithmetic =
    let private noObjectReferences() = invalidOp "Cannot use object reference in an arithmetic operation"

    let private pointerSizeMismatch expected actual =
        sprintf
            "Attempt to perform pointer arithmetic with mismatching pointee size, expected %i but got %i"
            expected
            actual
        |> invalidOp

    let inline private binop opu8 ops8 opu16 ops16 opu32 ops32 opu64 ops64 opunative opsnative opf32 opf64 rtype (xreg: inref<_>) (yreg: inref<_>) =
        match rtype with
        | RegisterType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool) ->
            Register.ofValue rtype (opu8 (ConvertRegister.u8 &xreg) (ConvertRegister.u8 &yreg))
        | RegisterType.Primitive PrimitiveType.S8 ->
            Register.ofValue rtype (ops8 (ConvertRegister.s8 &xreg) (ConvertRegister.s8 &yreg))
        | RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16) ->
            Register.ofValue rtype (opu16 (ConvertRegister.u16 &xreg) (ConvertRegister.u16 &yreg))
        | RegisterType.Primitive PrimitiveType.S16 ->
            Register.ofValue rtype (ops16 (ConvertRegister.s16 &xreg) (ConvertRegister.s16 &yreg))
        | RegisterType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32) ->
            Register.ofValue rtype (opu32 (ConvertRegister.u32 &xreg) (ConvertRegister.u32 &yreg))
        | RegisterType.Primitive PrimitiveType.S32 ->
            Register.ofValue rtype (ops32 (ConvertRegister.s32 &xreg) (ConvertRegister.s32 &yreg))
        | RegisterType.Primitive PrimitiveType.U64 ->
            Register.ofValue rtype (opu64 (ConvertRegister.u64 &xreg) (ConvertRegister.u64 &yreg))
        | RegisterType.Primitive PrimitiveType.S64 ->
            Register.ofValue rtype (ops64 (ConvertRegister.s64 &xreg) (ConvertRegister.s64 &yreg))
        | RegisterType.Primitive PrimitiveType.UNative ->
            Register.ofValue rtype (opunative (ConvertRegister.unative &xreg) (ConvertRegister.unative &yreg))
        | RegisterType.Primitive PrimitiveType.SNative ->
            Register.ofValue rtype (opsnative (ConvertRegister.snative &xreg) (ConvertRegister.snative &yreg))
        | RegisterType.Primitive PrimitiveType.F32 ->
            Register.ofValue rtype (opf32 (ConvertRegister.f32 &xreg) (ConvertRegister.f32 &yreg))
        | RegisterType.Primitive PrimitiveType.F64 ->
            Register.ofValue rtype (opf64 (ConvertRegister.f64 &xreg) (ConvertRegister.f64 &yreg))
        | RegisterType.Pointer size ->
            match xreg.Type, yreg.Type with
            | RegisterType.Pointer _, RegisterType.Pointer _ ->
                invalidOp "Arithmetic operations with multiple pointer types are prohibited"
            | RegisterType.Pointer xsize, _ when xsize = size ->
                let x = InterpretRegister.value<unativeint> &xreg
                let y = Checked.(*) (ConvertRegister.unative &yreg) (unativeint size)
                Register.ofValue rtype (opunative x y)
            | _, RegisterType.Pointer ysize when ysize = size ->
                let x = Checked.(*) (ConvertRegister.unative &xreg) (unativeint size)
                let y = InterpretRegister.value<unativeint> &yreg
                Register.ofValue rtype (opunative x y)
            | RegisterType.Pointer bad, _
            | _, RegisterType.Pointer bad ->
                pointerSizeMismatch size bad
            | _, _ ->
                Register.ofValue rtype (opunative (ConvertRegister.unative &xreg) (ConvertRegister.unative &yreg))
        | RegisterType.Object -> noObjectReferences()

    let add rtype (xreg: inref<_>) (yreg: inref<_>) = binop (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) rtype &xreg &yreg
    let sub rtype (xreg: inref<_>) (yreg: inref<_>) = binop (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) rtype &xreg &yreg
    let mul rtype (xreg: inref<_>) (yreg: inref<_>) = binop (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) rtype &xreg &yreg

    let inline private integerDivideOperation op x y =
        if y <> LanguagePrimitives.GenericZero
        then op x y
        else raise(NotImplementedException "Integer division that cannot throw on division by zero is not implemented")

    let div rtype (xreg: inref<_>) (yreg: inref<_>) =
        binop (integerDivideOperation (/)) (integerDivideOperation (/)) (integerDivideOperation (/))
            (integerDivideOperation (/)) (integerDivideOperation (/)) (integerDivideOperation (/))
            (integerDivideOperation (/)) (integerDivideOperation (/)) (integerDivideOperation (/))
            (integerDivideOperation (/)) (/) (/) rtype &xreg &yreg

    let rem rtype (xreg: inref<_>) (yreg: inref<_>) =
        binop (integerDivideOperation (%)) (integerDivideOperation (%)) (integerDivideOperation (%))
            (integerDivideOperation (%)) (integerDivideOperation (%)) (integerDivideOperation (%))
            (integerDivideOperation (%)) (integerDivideOperation (%)) (integerDivideOperation (%))
            (integerDivideOperation (%)) (/) (/) rtype &xreg &yreg

    let inline private bbinop opu8 ops8 opu16 ops16 opu32 ops32 opu64 ops64 opunative opsnative rtype (xreg: inref<_>) (yreg: inref<_>) =
        match rtype with
        | RegisterType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool) ->
            Register.ofValue rtype (opu8 (ConvertRegister.u8 &xreg) (ConvertRegister.u8 &yreg))
        | RegisterType.Primitive PrimitiveType.S8 ->
            Register.ofValue rtype (ops8 (ConvertRegister.s8 &xreg) (ConvertRegister.s8 &yreg))
        | RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16) ->
            Register.ofValue rtype (opu16 (ConvertRegister.u16 &xreg) (ConvertRegister.u16 &yreg))
        | RegisterType.Primitive PrimitiveType.S16 ->
            Register.ofValue rtype (ops16 (ConvertRegister.s16 &xreg) (ConvertRegister.s16 &yreg))
        | RegisterType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32 | PrimitiveType.F32) ->
            Register.ofValue rtype (opu32 (ConvertRegister.u32 &xreg) (ConvertRegister.u32 &yreg))
        | RegisterType.Primitive PrimitiveType.S32 ->
            Register.ofValue rtype (ops32 (ConvertRegister.s32 &xreg) (ConvertRegister.s32 &yreg))
        | RegisterType.Primitive(PrimitiveType.U64 | PrimitiveType.F64) ->
            Register.ofValue rtype (opu64 (ConvertRegister.u64 &xreg) (ConvertRegister.u64 &yreg))
        | RegisterType.Primitive PrimitiveType.S64 ->
            Register.ofValue rtype (ops64 (ConvertRegister.s64 &xreg) (ConvertRegister.s64 &yreg))
        | RegisterType.Primitive PrimitiveType.UNative | RegisterType.Pointer _ ->
            Register.ofValue rtype (opunative (ConvertRegister.unative &xreg) (ConvertRegister.unative &yreg))
        | RegisterType.Primitive PrimitiveType.SNative ->
            Register.ofValue rtype (opsnative (ConvertRegister.snative &xreg) (ConvertRegister.snative &yreg))
        | RegisterType.Object -> noObjectReferences()

    let ``and`` vtype (xreg: inref<_>) (yreg: inref<_>) =
        bbinop (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (RegisterType.Primitive vtype) &xreg &yreg

    let ``or`` vtype (xreg: inref<_>) (yreg: inref<_>) =
        bbinop (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) (RegisterType.Primitive vtype) &xreg &yreg

    let xor vtype (xreg: inref<_>) (yreg: inref<_>) =
        bbinop (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (RegisterType.Primitive vtype) &xreg &yreg

    let inline private unop opu8 ops8 opu16 ops16 opu32 ops32 opu64 ops64 opunative opsnative opf32 opf64 rtype (register: inref<_>) =
        match rtype with
        | RegisterType.Primitive PrimitiveType.SNative ->
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
        | RegisterType.Pointer size ->
            match register.Type with
            | RegisterType.Pointer rsize when size = rsize ->
                Register.ofValue rtype (opunative(Checked.(*) (InterpretRegister.value<unativeint> &register) (unativeint size)))
            | _ ->
                Register.ofValue rtype (opunative(InterpretRegister.value<unativeint> &register))
            | RegisterType.Pointer rsize ->
                pointerSizeMismatch size rsize

    let inline private oneop (op: _ -> _ -> _) = op LanguagePrimitives.GenericOne

    let inline private inc value = oneop (+) value
    let incr rtype (register: inref<_>) = unop inc inc inc inc inc inc inc inc inc inc inc inc rtype &register
    let inline private dec value = oneop (-) value
    let decr rtype (register: inref<_>) = unop dec dec dec dec dec dec dec dec dec dec dec dec rtype &register

    let inline private bunop opu8 ops8 opu16 ops16 opu32 ops32 opu64 ops64 opunative opsnative rtype (register: inref<_>) =
        match rtype with
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
        | RegisterType.Pointer _ | RegisterType.Primitive PrimitiveType.UNative ->
            Register.ofValue rtype (opunative(InterpretRegister.value<unativeint> &register))
        | RegisterType.Primitive PrimitiveType.SNative ->
            Register.ofValue rtype (opsnative(InterpretRegister.value<nativeint> &register))
        | RegisterType.Object -> noObjectReferences()

    let ``not`` rtype (register: inref<_>) =
        bunop (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (RegisterType.Primitive rtype) &register

    [<RequireQualifiedAccess>]
    module Checked =
        open Microsoft.FSharp.Core.Operators.Checked

        let add rtype (xreg: inref<_>) (yreg: inref<_>) = binop (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) rtype &xreg &yreg
        let sub rtype (xreg: inref<_>) (yreg: inref<_>) = binop (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) rtype &xreg &yreg
        let mul rtype (xreg: inref<_>) (yreg: inref<_>) = binop (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) rtype &xreg &yreg

        let inline private floatDivOp op x y =
            if y <> LanguagePrimitives.GenericZero
            then op x y
            else raise(DivideByZeroException())

        let div vtype (xreg: inref<_>) (yreg: inref<_>) =
            binop (/) (/) (/) (/) (/) (/) (/) (/) (/) (/) (floatDivOp (/)) (floatDivOp (/)) vtype &xreg &yreg

        let rem vtype (xreg: inref<_>) (yreg: inref<_>) =
            binop (%) (%) (%) (%) (%) (%) (%) (%) (%) (%) (floatDivOp (%)) (floatDivOp (%)) vtype &xreg &yreg

        let inline private inc value = oneop (+) value
        let incr vtype (register: inref<_>) = unop inc inc inc inc inc inc inc inc inc inc inc inc vtype &register
        let inline private dec value = oneop (-) value
        let decr vtype (register: inref<_>) = unop dec dec dec dec dec dec dec dec dec dec dec dec vtype &register

    let inline private rotop sh8 sh16 sh32 sh64 shnative itype (register: inref<Register>) amount =
        let rtype = RegisterType.primitive itype
        match itype with
        | PrimitiveType.S8 | PrimitiveType.U8 | PrimitiveType.Bool ->
            Register.ofValue rtype (sh8 (ConvertRegister.u8 &register) amount)
        | PrimitiveType.S16 | PrimitiveType.U16 | PrimitiveType.Char16 ->
            Register.ofValue rtype (sh16 (ConvertRegister.u16 &register) amount)
        | PrimitiveType.S32 | PrimitiveType.U32 | PrimitiveType.Char32 | PrimitiveType.F32 ->
            Register.ofValue rtype (sh32 (ConvertRegister.u32 &register) amount)
        | PrimitiveType.S64 | PrimitiveType.U64 | PrimitiveType.F64 ->
            Register.ofValue rtype (sh64 (ConvertRegister.u64 &register) amount)
        | PrimitiveType.UNative | PrimitiveType.SNative ->
            Register.ofValue rtype (shnative (ConvertRegister.unative &register) amount)

    let rotl itype (register: inref<_>) amount =
        rotop
            (fun value amount -> (value <<< amount) ||| (value >>> (8 - amount)))
            (fun value amount -> (value <<< amount) ||| (value >>> (16 - amount)))
            (fun value amount -> System.Numerics.BitOperations.RotateLeft(value, amount))
            (fun value amount -> System.Numerics.BitOperations.RotateLeft(value, amount))
            (fun value amount -> (value <<< amount) ||| (value >>> (sizeof<unativeint> - amount)))
            itype
            &register
            amount

    let rotr itype (register: inref<_>) amount =
           rotop
               (fun value amount -> (value >>> amount) ||| (value <<< (8 - amount)))
               (fun value amount -> (value >>> amount) ||| (value <<< (16 - amount)))
               (fun value amount -> System.Numerics.BitOperations.RotateRight(value, amount))
               (fun value amount -> System.Numerics.BitOperations.RotateRight(value, amount))
               (fun value amount -> (value >>> amount) ||| (value <<< (sizeof<unativeint> - amount)))
               itype
               &register
               amount

/// Contains functions for comparing the values stored in registers.
[<RequireQualifiedAccess>]
module private RegisterComparison =
    let isTrueValue (register: inref<Register>) =
        match register.Type with
        | RegisterType.Primitive PrimitiveType.Bool -> InterpretRegister.value<bool> &register
        | RegisterType.Primitive PrimitiveType.U8 -> InterpretRegister.value<uint8> &register <> 0uy
        | RegisterType.Primitive PrimitiveType.S8 -> InterpretRegister.value<int8> &register <> 0y
        | RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16) -> InterpretRegister.value<uint16> &register <> 0us
        | RegisterType.Primitive PrimitiveType.S16 -> InterpretRegister.value<int16> &register <> 0s
        | RegisterType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32)
        | RegisterType.Primitive PrimitiveType.F32 -> InterpretRegister.value<uint32> &register <> 0u
        | RegisterType.Primitive PrimitiveType.S32 -> InterpretRegister.value<int32> &register <> 0
        | RegisterType.Primitive(PrimitiveType.U64 | PrimitiveType.F64) -> InterpretRegister.value<uint64> &register <> 0UL
        | RegisterType.Primitive PrimitiveType.S64 -> InterpretRegister.value<int64> &register <> 0L
        | RegisterType.Primitive PrimitiveType.UNative
        | RegisterType.Pointer _ -> InterpretRegister.value<unativeint> &register <> 0un
        | RegisterType.Primitive PrimitiveType.SNative -> InterpretRegister.value<nativeint> &register <> 0n
        | RegisterType.Object -> InterpretRegister.value<ObjectReference> &register <> ObjectReference.Null

    let inline private comparison s8 u8 s16 u16 s32 u32 s64 u64 snative unative f32 f64 (xreg: inref<Register>) (yreg: inref<Register>) =
        match xreg.Type, yreg.Type with
        | (RegisterType.Pointer  _| RegisterType.Object | RegisterType.Primitive PrimitiveType.SNative),
          (RegisterType.Pointer  _| RegisterType.Object | RegisterType.Primitive PrimitiveType.SNative) ->
            snative (InterpretRegister.value<nativeint> &xreg) (InterpretRegister.value<nativeint> &yreg)
        | (RegisterType.Object _, _) | (_, RegisterType.Object) ->
            invalidOp "Comparing an object reference and a numeric value is prohibited"
        | (RegisterType.Primitive PrimitiveType.SNative | RegisterType.Pointer _), RegisterType.Primitive PrimitiveType.UNative ->
            let x = InterpretRegister.value<nativeint> &xreg
            if x < 0n
            then failwith "TODO: How to compare long and ulong?"
            else unative (unativeint x) (InterpretRegister.value<unativeint> &yreg)
        | RegisterType.Primitive PrimitiveType.UNative, (RegisterType.Primitive PrimitiveType.SNative | RegisterType.Pointer _) ->
            let y = InterpretRegister.value<nativeint> &yreg
            if y < 0n
            then failwith "TODO: How to compare long and ulong?"
            else unative (InterpretRegister.value<unativeint> &xreg) (unativeint y)
        | _, RegisterType.Primitive PrimitiveType.F64 ->
            f64 (ConvertRegister.f64 &xreg) (InterpretRegister.value<double> &yreg)
        | RegisterType.Primitive PrimitiveType.F64, _ ->
            f64 (InterpretRegister.value<double> &xreg) (ConvertRegister.f64 &yreg)
        | RegisterType.Primitive PrimitiveType.F32, _ ->
            f32 (InterpretRegister.value<single> &xreg) (ConvertRegister.f32 &yreg)
        | _, RegisterType.Primitive PrimitiveType.F32 ->
            f32 (ConvertRegister.f32 &xreg) (InterpretRegister.value<single> &yreg)
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
            u64 (InterpretRegister.value<uint64> &xreg) (ConvertRegister.u64 &yreg)
        | _, RegisterType.Primitive PrimitiveType.U64 ->
            u64 (ConvertRegister.u64 &xreg) (InterpretRegister.value<uint64> &yreg)
        | RegisterType.Primitive PrimitiveType.S64, _ ->
            s64 (InterpretRegister.value<int64> &xreg) (ConvertRegister.s64 &yreg)
        | _, RegisterType.Primitive PrimitiveType.S64 ->
            s64 (ConvertRegister.s64 &xreg) (InterpretRegister.value<int64> &yreg)
        | RegisterType.Primitive PrimitiveType.UNative, _ ->
            unative (InterpretRegister.value<unativeint> &xreg) (ConvertRegister.unative &yreg)
        | _, RegisterType.Primitive PrimitiveType.UNative ->
            unative (ConvertRegister.unative &xreg) (InterpretRegister.value<unativeint> &yreg)
        | (RegisterType.Primitive PrimitiveType.SNative | RegisterType.Pointer _), _ ->
            snative (InterpretRegister.value<nativeint> &xreg) (ConvertRegister.snative &yreg)
        | _, (RegisterType.Primitive PrimitiveType.SNative | RegisterType.Pointer _) ->
            snative (ConvertRegister.snative &xreg) (InterpretRegister.value<nativeint> &yreg)
        | RegisterType.Primitive PrimitiveType.S32, RegisterType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32)
        | RegisterType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32), RegisterType.Primitive PrimitiveType.S32 ->
            s64 (ConvertRegister.s64 &xreg) (ConvertRegister.s64 &yreg)
        | RegisterType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32), _ ->
            u32 (InterpretRegister.value<uint32> &xreg) (ConvertRegister.u32 &yreg)
        | _, RegisterType.Primitive(PrimitiveType.U32 | PrimitiveType.Char32) ->
            u32 (ConvertRegister.u32 &xreg) (InterpretRegister.value<uint32> &yreg)
        | RegisterType.Primitive PrimitiveType.S32, _ ->
            s32 (InterpretRegister.value<int32> &xreg) (ConvertRegister.s32 &yreg)
        | _, RegisterType.Primitive PrimitiveType.S32 ->
            s32 (ConvertRegister.s32 &xreg) (InterpretRegister.value<int32> &yreg)
        | RegisterType.Primitive PrimitiveType.S16, RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16)
        | RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16), RegisterType.Primitive PrimitiveType.S16 ->
            s32 (ConvertRegister.s32 &xreg) (ConvertRegister.s32 &yreg)
        | RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16), _ ->
            u16 (InterpretRegister.value<uint16> &xreg) (ConvertRegister.u16 &yreg)
        | _, RegisterType.Primitive(PrimitiveType.U16 | PrimitiveType.Char16) ->
            u16 (ConvertRegister.u16 &xreg) (InterpretRegister.value<uint16> &yreg)
        | RegisterType.Primitive PrimitiveType.S16, _ ->
            s16 (InterpretRegister.value<int16> &xreg) (ConvertRegister.s16 &yreg)
        | _, RegisterType.Primitive PrimitiveType.S16 ->
            s16 (ConvertRegister.s16 &xreg) (InterpretRegister.value<int16> &yreg)
        | RegisterType.Primitive PrimitiveType.S8, RegisterType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool)
        | RegisterType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool), RegisterType.Primitive PrimitiveType.S8 ->
            s16 (ConvertRegister.s16 &xreg) (ConvertRegister.s16 &yreg)
        | RegisterType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool), RegisterType.Primitive(PrimitiveType.U8 | PrimitiveType.Bool) ->
            u8 (InterpretRegister.value<uint8> &xreg) (InterpretRegister.value<uint8> &yreg)
        | RegisterType.Primitive PrimitiveType.S8, RegisterType.Primitive PrimitiveType.S8 ->
            s8 (InterpretRegister.value<int8> &xreg) (InterpretRegister.value<int8> &yreg)

    let isLessThan (xreg: inref<_>) (yreg: inref<_>) =
        comparison (<) (<) (<) (<) (<) (<) (<) (<) (<) (<) (<) (<) &xreg &yreg

    let isGreaterThan (xreg: inref<_>) (yreg: inref<_>) =
        comparison (>) (>) (>) (>) (>) (>) (>) (>) (>) (>) (>) (>) &xreg &yreg

    let isEqual (xreg: inref<_>) (yreg: inref<_>) =
        comparison (=) (=) (=) (=) (=) (=) (=) (=) (=) (=) (=) (=) &xreg &yreg

    let isLessOrEqual (xreg: inref<_>) (yreg: inref<_>) =
        comparison (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) &xreg &yreg

    let isGreaterOrEqual (xreg: inref<_>) (yreg: inref<_>) =
        comparison (>=) (>=) (>=) (>=) (>=) (<=) (<=) (<=) (<=) (<=) (<=) (<=) &xreg &yreg

type ExternalCallHandler =
    delegate of IGarbageCollector * IGarbageCollectionState<IEnumerator<ObjectReference>> * TypeSizeResolver *
        ObjectTypeResolver * ObjectTypeLookup * TypeLayoutResolver * StackFrame -> unit

[<RequireQualifiedAccess>]
module ExternalCode =
    [<Literal>]
    let private InternalCall = "runmdl"

    let private lookup = Dictionary<struct(string * string), ExternalCallHandler>()

    let private println gc _ _ _ objectTypeLookup _ (frame: StackFrame) =
        let arg = frame.ArgumentRegisters.[0]
        match arg.Type with
        | RegisterType.Object ->
            let str = InterpretRegister.value<ObjectReference> &arg
            let length = ArrayObject.length str
            let address = ArrayObject.address str
            match ArrayObject.getElementType gc objectTypeLookup str with
            | _, AnyType.ValueType(ValueType.Primitive PrimitiveType.Char32) ->
                for rune in Span<System.Text.Rune>(address, length) do stdout.Write(rune.ToString())
            | _, AnyType.ValueType(ValueType.Primitive PrimitiveType.Char16) ->
                for c in Span<char>(address, length) do stdout.Write c
            | _, bad ->
                raise(ArgumentException(sprintf "%A is not a valid character type" bad))
        | RegisterType.Pointer 4u ->
            let mutable address = InterpretRegister.value<nativeptr<System.Text.Rune>> &arg
            while NativePtr.read address <> Unchecked.defaultof<_> do
                stdout.Write((NativePtr.read address).ToString())
                address <- NativePtr.add address 1
        | bad -> raise(ArgumentException(sprintf "%A is not a valid message argument" bad))
        stdout.WriteLine()

    do lookup.[(InternalCall, "testhelperprintln")] <- ExternalCallHandler println
    do lookup.[(InternalCall, "GC_Collect")] <- ExternalCallHandler(fun gc state _ _ _ _ _ -> gc.Collect state)
    do lookup.[(InternalCall, "break")] <- ExternalCallHandler(fun _ _ _ _ _ _ _ -> System.Diagnostics.Debugger.Launch() |> ignore)

    let call library name =
        match lookup.TryGetValue(struct(library, name)) with
        | true, call' -> call'
        | false, _ -> raise(NotImplementedException(sprintf "TODO: Handle external calls to %s in %s" library name))

let private setupStackFrame
    typeSizeResolver
    (method: ResolvedMethod)
    (frame: StackFrame voption ref)
    (runExternalCode: byref<_ voption>)
    =
    let inline createRegisterList (indices: ImmutableArray<_>) = Array.init indices.Length <| fun i ->
        Register.ofTypeIndex typeSizeResolver method.DeclaringModule indices.[i]

    let args = createRegisterList method.Signature.ParameterTypes
    let returns = createRegisterList method.Signature.ReturnTypes

    match method.Body with
    | MethodBody.Defined codei ->
        let code = method.DeclaringModule.CodeAt codei
        if code.Blocks.IsDefaultOrEmpty then invalidOp(sprintf "Cannot invoke %O, no code blocks are defined" method)
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

    let (|MemoryAccess|) mask (flags: MemoryAccessFlags) =
        if flags &&& (~~~mask) <> MemoryAccessFlags.None then
            failwithf "TODO: Bad memory access flags %A" flags
        flags

    let (|Call|) mask (flags: CallFlags) =
        if flags &&& (~~~mask) <> CallFlags.None then
            failwithf "TODO: Bad call flags %A" flags
        flags

[<RequireQualifiedAccess>]
module private ObjectField =
    let checkCanMutate (field: ResolvedField) (frame: StackFrame) =
        if not field.IsMutable && not frame.CurrentMethod.IsConstructor then
            invalidOp "Attempted to modify read-only field outside of constructor or type initializer"

    let address { TypeLayout.Fields = layouts } (object: voidptr) field =
        NativePtr.toVoidPtr(NativePtr.add (NativePtr.ofVoidPtr<byte> object) layouts.[field])

    let access gc objectTypeLookup typeLayoutResolver (typeSizeResolver: TypeSizeResolver) (object: inref<Register>) field =
        match object.Type with
        | RegisterType.Object ->
            // TODO: If field contains a struct, perform struct copying
            let o = InterpretRegister.value<ObjectReference> &object
            // TODO: Create helper to convert from ObjectType to TypeLayout
            let addr =
                address
                    (TypeLayout.ofObjectReference gc objectTypeLookup typeLayoutResolver o)
                    (ObjectReference.toVoidPtr o)
                    field

            Span<byte>(addr, typeSizeResolver field.DeclaringModule field.FieldType)
        | RegisterType.Pointer _->
            raise(NotImplementedException "TODO: Accessing of fields using pointers is not yet supported")
            Span()
        | RegisterType.Primitive _ ->
            invalidOp "Cannot access field using a primitive value"
            Span()

[<RequireQualifiedAccess>]
module private MemoryOperations =
    let alloca (stack: ValueStack) size count =
        // TODO: If multiple object references are allocated on the stack, don't forget to root them.
        let mutable address = Unchecked.defaultof<_>
        if stack.TryAllocate(Checked.(*) size count, &address) then
            address
        else
            Unchecked.defaultof<voidptr>

    let store (typeSizeResolver: TypeSizeResolver) destination (source: inref<Register>) (rm: ResolvedModule) ty =
        let inline write value = NativePtr.write (NativePtr.ofVoidPtr destination) value
        match ty, source.Type with
        | AnyType.ValueType(ValueType.Primitive prim), RegisterType.Primitive _ ->
            match prim with
            | PrimitiveType.Bool | PrimitiveType.U8 -> write(ConvertRegister.u8 &source)
            | PrimitiveType.S8 -> write(ConvertRegister.s8 &source)
            | PrimitiveType.U16 | PrimitiveType.Char16 -> write(ConvertRegister.u16 &source)
            | PrimitiveType.S16 -> write(ConvertRegister.s16 &source)
            | PrimitiveType.U32 | PrimitiveType.Char32 -> write(ConvertRegister.u32 &source)
            | PrimitiveType.S32 -> write(ConvertRegister.s32 &source)
            | PrimitiveType.U64 -> write(ConvertRegister.u64 &source)
            | PrimitiveType.S64 -> write(ConvertRegister.s64 &source)
            | PrimitiveType.UNative -> write(ConvertRegister.unative &source)
            | PrimitiveType.SNative -> write(ConvertRegister.snative &source)
            | PrimitiveType.F32 -> write(ConvertRegister.f32 &source)
            | PrimitiveType.F64 -> write(ConvertRegister.f64 &source)
        | (AnyType.SafePointer _ | AnyType.ReferenceType _ | AnyType.ValueType(ValueType.UnsafePointer _)), (RegisterType.Pointer _ | RegisterType.Object) ->
            write(InterpretRegister.value<nativeptr<byte>> &source)
        | AnyType.ValueType(ValueType.Defined _), (RegisterType.Pointer _ | RegisterType.Object) ->
            let size = typeSizeResolver rm ty
            let address = NativePtr.toVoidPtr(InterpretRegister.value<nativeptr<byte>> &source)
            Span<byte>(address, size).CopyTo(Span<byte>(destination, size))
        | _ ->
            invalidOp(sprintf "Cannot store a value stored in a register of type %A into a destination containing a %A" ty source.Type)

    let load stack (typeSizeResolver: TypeSizeResolver) source rm ty =
        let inline read() = NativePtr.read (NativePtr.ofVoidPtr source)
        match ty with
        | AnyType.ValueType(ValueType.Primitive prim) ->
            let rtype = RegisterType.Primitive prim
            match prim with
            | PrimitiveType.Bool | PrimitiveType.U8 | PrimitiveType.S8 ->
                Register.ofValue<uint8> rtype (read())
            | PrimitiveType.U16 | PrimitiveType.Char16 | PrimitiveType.S16 ->
                Register.ofValue<uint16> rtype (read())
            | PrimitiveType.U32 | PrimitiveType.Char32 | PrimitiveType.S32 | PrimitiveType.F32 ->
                Register.ofValue<uint32> rtype (read())
            | PrimitiveType.U64 | PrimitiveType.S64 | PrimitiveType.F64 ->
                Register.ofValue<uint64> rtype (read())
            | PrimitiveType.UNative | PrimitiveType.SNative ->
                Register.ofValue<unativeint> rtype (read())
        | AnyType.ValueType(ValueType.UnsafePointer vtype)
        | AnyType.SafePointer(ReferenceOrValueType.Value vtype) ->
            let size = typeSizeResolver rm (AnyType.ValueType vtype)
            Register.ofValue<nativeptr<byte>> (RegisterType.Pointer(uint32 size)) (read())
        | AnyType.SafePointer(ReferenceOrValueType.Reference _) ->
            Register.ofValue<nativeptr<ObjectReference>> (RegisterType.Pointer(uint32 sizeof<ObjectReference>)) (read())
        | AnyType.ReferenceType _ ->
            Register.ofValue<ObjectReference> RegisterType.Object (read())
        | AnyType.ValueType(ValueType.Defined _) ->
            let size = typeSizeResolver rm ty
            let destination = alloca stack size 1
            Span<byte>(source, size).CopyTo(Span<byte>(destination, size))
            Register.ofValue<nativeptr<byte>> (RegisterType.Pointer(uint32 size)) (NativePtr.ofVoidPtr destination)

[<Sealed>]
type EventSource () =
    let called = Event<StackFrame>()
    let returned = Event<StackFrame>()

    [<CLIEvent>] member _.MethodCalled = called.Publish
    [<CLIEvent>] member _.MethodReturned = returned.Publish

    member _.TriggerMethodCall frame = called.Trigger frame
    member _.TriggerMethodReturn frame = returned.Trigger frame

[<RequireQualifiedAccess; Struct; NoComparison; NoEquality>]
type private RootObjects =
    val mutable Temporaries: ImmutableArray<AllocatedRegister>.Builder
    val mutable Locals: ImmutableArray<AllocatedRegister>.Builder
    val mutable Addressed: List<stackptr<ObjectReference>>

[<Sealed>]
type RootObjectCollection () =
    let roots = ImmutableArray.CreateBuilder<RootObjects>()

    let enumerated = seq {
        for i = 0 to roots.Count - 1 do
            let objects = (*&*)roots.ItemRef i
            if objects.Temporaries <> null then
                for registeri = 0 to objects.Temporaries.Count - 1 do
                    yield InterpretRegister.value<ObjectReference> (&objects.Temporaries.ItemRef(registeri).Register)
            if objects.Locals <> null then
                for registeri = 0 to objects.Locals.Count - 1 do
                    yield InterpretRegister.value<ObjectReference> (&objects.Locals.ItemRef(registeri).Register)
            if objects.Addressed <> null then
                for address in objects.Addressed do yield StackPtr.read address
    }

    member private _.Current = &roots.ItemRef(roots.Count - 1)
    member private this.CurrentRef = &Unsafe.AsRef &this.Current // Not safe if roots were resized.

    member this.RootTemporary(temporary: AllocatedRegister) =
        match temporary.Register.Type with
        | RegisterType.Object ->
            if isNull this.Current.Temporaries then this.CurrentRef.Temporaries <- ImmutableArray.CreateBuilder()
            this.Current.Temporaries.Add temporary
        // Don't need to check for roots if register contains pointer, as alloca already updates the root collection.
        | RegisterType.Pointer _
        | RegisterType.Primitive _ -> ()

    member this.ClearTemporaries() =
        if this.Current.Temporaries <> null then this.Current.Temporaries.Clear()

    member this.RootLocals(locals: byref<#IEnumerator<AllocatedRegister>>) =
        if isNull this.Current.Locals then this.CurrentRef.Locals <- ImmutableArray.CreateBuilder()
        while locals.MoveNext() do
            let register = locals.Current
            match register.Register.Type with
            | RegisterType.Object ->
                this.Current.Locals.Add register
            // Don't need to check for roots if register contains pointer, as alloca already updates the root collection.
            | RegisterType.Pointer _
            | RegisterType.Primitive _ -> ()

    member this.ClearLocals() =
        if this.Current.Locals <> null then this.Current.Locals.Clear()

    /// Includes an object reference on the stack as a root.
    member this.RootAddress address =
        if isNull this.Current.Addressed then this.CurrentRef.Addressed <- List()
        this.Current.Addressed.Add address

    member _.PopRoots() = roots.RemoveAt(roots.Count - 1)
    member _.PushRoots() = roots.Add(RootObjects())

    member _.GetEnumerator() = // TODO: Create a custom struct root enumerator to avoid large allocations every single time the interpreter's GC is run.
        enumerated.GetEnumerator()

    // TODO: Maybe supply a dictionary to this method instead, this is O(very bad) right now when lots of objects are relocated.
    member _.AdjustMovedObjects (relocations: byref<#IEnumerator<ObjectRelocation>>) =
        let adjustObjectRegisters (update: ObjectRelocation) (registers: ImmutableArray<AllocatedRegister>.Builder) =
            if registers <> null then
                for i = 0 to registers.Count - 1 do
                    let register = &registers.ItemRef(i).Register
                    if InterpretRegister.value<ObjectReference> &register = update.From then
                        Unsafe.As<RegisterValue, ObjectReference>(&Unsafe.AsRef &register.Value) <- update.To

        while relocations.MoveNext() do
            let update = relocations.Current
            for i = 0 to roots.Count - 1 do
                let stackRootObjects = &roots.ItemRef i

                adjustObjectRegisters update stackRootObjects.Temporaries

                adjustObjectRegisters update stackRootObjects.Locals

                if stackRootObjects.Addressed <> null then
                    for stackObjectReference in stackRootObjects.Addressed do
                        if StackPtr.read stackObjectReference = update.From then StackPtr.write stackObjectReference update.To

let private noAnyObject() = invalidOp "Unexpected object reference of unspecified type"

let private expectedReferenceType actual = invalidOp(sprintf "Expected object reference to be of a reference type, but got %A" actual)

[<Sealed>]
type GarbageCollectionState
    (
        typeSizeResolver: TypeSizeResolver,
        objectTypeLookup: ObjectTypeLookup,
        typeLayoutResolver: TypeLayoutResolver
    )
    =
    member val Roots = RootObjectCollection()

    member private _.GetReferencedObjects(gc: IGarbageCollector, o) =
        let struct(rm, ty) = objectTypeLookup(gc.TypeOf o)
        match ty with
        | AnyType.ReferenceType(ReferenceType.Defined typei)
        | AnyType.ReferenceType(ReferenceType.BoxedValueType(ValueType.Defined typei)) ->
            let layout = typeLayoutResolver(rm.TypeAt typei)
            if not layout.References.IsDefaultOrEmpty then
                let (ObjectReference address) = o
                let mutable references = Array.zeroCreate layout.References.Length
                for i = 0 to references.Length - 1 do
                    references.[i] <-
                        address + nativeint layout.References.[i]
                        |> NativePtr.ofNativeInt<ObjectReference>
                        |> NativePtr.read
                Unsafe.As<ObjectReference[], _> &references
            else
                ImmutableArray<ObjectReference>.Empty
        | AnyType.ReferenceType(ReferenceType.Vector(ReferenceOrValueType.Reference _)) ->
            match ArrayObject.length o with
            | 0 -> ImmutableArray.Empty
            | length ->
                let mutable elements = Array.zeroCreate length
                let address = NativePtr.ofVoidPtr<ObjectReference>(ArrayObject.address o)
                for offset = 0 to length - 1 do elements.[offset] <- NativePtr.read(NativePtr.add address offset)
                Unsafe.As<ObjectReference[], _> &elements
        | AnyType.ReferenceType(ReferenceType.Vector(ReferenceOrValueType.Value(ValueType.Defined typei))) ->
            let layout = typeLayoutResolver(rm.TypeAt typei)
            match layout.References.Length, ArrayObject.length o with
            | 0, _ | _, 0 -> ImmutableArray.Empty
            | reflen, arrlen ->
                let mutable references = Array.zeroCreate(arrlen * reflen)
                let start = NativePtr.ofVoidPtr<byte>(ArrayObject.address o)
                for i = 0 to arrlen - 1 do
                    let element = NativePtr.add start (i * layout.Size)
                    for offset = 0 to reflen - 1 do
                        references.[i + offset] <-
                            NativePtr.toNativeInt element + nativeint layout.References.[offset]
                            |> NativePtr.ofNativeInt<ObjectReference>
                            |> NativePtr.read
                Unsafe.As<ObjectReference[], _> &references
        | AnyType.ReferenceType(ReferenceType.Vector(ReferenceOrValueType.Value(ValueType.Primitive _ | ValueType.UnsafePointer _)))
        | AnyType.ReferenceType(ReferenceType.BoxedValueType(ValueType.Primitive _ | ValueType.UnsafePointer _)) ->
            ImmutableArray<ObjectReference>.Empty
        | AnyType.ReferenceType ReferenceType.Any ->
            noAnyObject()
        | AnyType.ValueType _ | AnyType.SafePointer _ ->
            expectedReferenceType ty

    interface IGarbageCollectionState<IEnumerator<ObjectReference>> with
        member state.EnumerateRoots() = state.Roots.GetEnumerator()

        member _.GetTypeSize otype =
            let struct(rm, ty) = objectTypeLookup otype
            typeSizeResolver rm ty

        member state.GetReferencedObjects(gc, o) = state.GetReferencedObjects(gc, o)

        member state.AdjustMovedObjects relocations = state.Roots.AdjustMovedObjects &relocations

[<Struct; NoComparison; NoEquality>]
type private RegisterArrayEnumerator =
    val mutable private registers: Register[]
    val mutable private index: int32

    new (registers) = { registers = registers; index = -1 }

    member this.Current = AllocatedRegister(this.registers, this.index)

    interface IEnumerator<AllocatedRegister> with
        member this.Current = this.Current

    interface IDisposable with
        member _.Dispose() = ()

    interface System.Collections.IEnumerator with
        member this.MoveNext() =
            this.index <- this.index + 1
            this.index < this.registers.Length

        member this.Current = this.Current :> obj
        member this.Reset() = this.index <- -1

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ExceptionRegister = { Value: Register; Origin: StackFrame }

[<Sealed>]
type UncaughtProgramException (message: string, ex: ExceptionRegister) =
    inherit Exception(message)

    member _.ProgramFrame = ex.Origin

let interpret
    (gc: IGarbageCollector)
    (garbageCollectionState: GarbageCollectionState)
    maxStackCapacity
    (typeSizeResolver: TypeSizeResolver)
    (objectTypeResolver: ObjectTypeResolver)
    (objectTypeLookup: ObjectTypeLookup)
    (typeLayoutResolver: TypeLayoutResolver)
    (interpreterEventHandler: (EventSource -> unit) option)
    (stackEventHandler: _ option)
    (arguments: ImmutableArray<Register>)
    (entrypoint: ResolvedMethod)
    =
    let mutable frame: StackFrame voption ref = ref ValueNone
    let mutable runExternalCode: _ voption = ValueNone
    let mutable caughtProgramException = ValueNone
    let roots = garbageCollectionState.Roots
    use stack = new ValueStack(maxStackCapacity)

    if stackEventHandler.IsSome then
        stackEventHandler.Value stack

    let events =
        match interpreterEventHandler with
        | Some handler ->
            let source = EventSource()
            handler source
            ValueSome source
        | None -> ValueNone

    let invoke flags (arguments: ImmutableArray<Register>) (method: ResolvedMethod) =
        if isFlagSet CallFlags.RequiresTailCallOptimization flags then
            raise(NotImplementedException "Tail call optimization is not yet supported")

        stack.SaveAllocations()
        roots.PushRoots()
        setupStackFrame typeSizeResolver method frame &runExternalCode
        let current = frame.Value.Value
        if events.IsSome then events.Value.TriggerMethodCall current

        let newArgumentRegisters = current.ArgumentRegisters
        if arguments.Length < newArgumentRegisters.Length then
            invalidOp(sprintf "Expected %i arguments but only %i were provided" newArgumentRegisters.Length arguments.Length)

        // TODO: Copy only the Value part of the argument register.
        // TODO: Check that argument register types match.
        arguments.AsSpan().CopyTo(Span newArgumentRegisters)

        let mutable argumentRegisterEnumerator = new RegisterArrayEnumerator(newArgumentRegisters)
        roots.RootLocals &argumentRegisterEnumerator

        current.ReturnRegisters

    let entryPointResults = invoke CallFlags.None arguments entrypoint

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
        let control = frame.Value.Value

#if DEBUG
        let (|Register|) rindex =
#else
        let inline (|Register|) rindex =
#endif
            control.RegisterAt rindex

        let (|LookupRegisterArray|) (indices: ImmutableArray<RegisterIndex>) =
            let mutable registers = Array.zeroCreate indices.Length // TODO: Cache register lookup array.
            for i = 0 to registers.Length - 1 do registers.[i] <- control.RegisterAt indices.[i]
            Unsafe.As<_, ImmutableArray<Register>> &registers

        let inline (|Method|) mindex: ResolvedMethod = control.CurrentModule.MethodAt mindex
        let inline (|Field|) findex: ResolvedField = control.CurrentModule.FieldAt findex
        let inline (|TypeSignature|) tindex: AnyType = control.CurrentModule.TypeSignatureAt tindex
        let inline (|RegisterType|) (TypeSignature rtype) = anyTypeToRegisterType typeSizeResolver control.CurrentModule rtype
        //let inline (|TypeLayout|) (t: RuntimeTypeDefinition) = t.Layout
        let inline (|Data|) dindex: ImmutableArray<_> = control.CurrentModule.DataAt dindex
        let inline (|BranchTarget|) (target: BlockOffset) = Checked.(+) control.BlockIndex target

#if DEBUG
        let branchToTarget (BranchTarget target) =
#else
        let inline branchToTarget (BranchTarget target) =
#endif
            control.JumpTo target
            roots.ClearLocals()
            roots.ClearTemporaries()
            let mutable locals = control.PreviousLocalRegisters.Values.GetEnumerator()
            roots.RootLocals &locals

#if DEBUG
        let insertThisArgument (arguments: ImmutableArray<Register>) o =
#else
        let inline insertThisArgument (arguments: ImmutableArray<_>) o =
#endif
            arguments.Insert(0, Register.ofValue RegisterType.Object o)

        let returnBackControl current previous (results: ImmutableArray<_>) =
            if events.IsSome then events.Value.TriggerMethodReturn current
            frame.Value <- previous

            match previous with
            | ValueSome caller ->
                roots.PopRoots()
                for i = 0 to results.Length - 1 do
                    let register = AllocatedRegister(results, i)
                    roots.RootTemporary register
                    caller.TemporaryRegisters.Add register.Register
            | ValueNone -> ()

            stack.FreeAllocations()

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
                control.TemporaryRegisters.Add(if RegisterComparison.isTrueValue &condition then vtrue else vfalse)
            | Add(ValidFlags.Arithmetic flags, RegisterType rtype, Register x, Register y) ->
                if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                then RegisterArithmetic.Checked.add rtype &x &y
                else RegisterArithmetic.add rtype &x &y
                |> control.TemporaryRegisters.Add
            | Sub(ValidFlags.Arithmetic flags, RegisterType rtype, Register x, Register y) ->
                if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                then RegisterArithmetic.Checked.sub rtype &x &y
                else RegisterArithmetic.sub rtype &x &y
                |> control.TemporaryRegisters.Add
            | Mul(ValidFlags.Arithmetic flags, RegisterType rtype, Register x, Register y) ->
                if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                then RegisterArithmetic.Checked.mul rtype &x &y
                else RegisterArithmetic.mul rtype &x &y
                |> control.TemporaryRegisters.Add
            | Div(ValidFlags.Arithmetic flags, RegisterType vtype, Register x, Register y) ->
                if isFlagSet ArithmeticFlags.ThrowOnDivideByZero flags
                then RegisterArithmetic.Checked.div vtype &x &y
                else RegisterArithmetic.div vtype &x &y
                |> control.TemporaryRegisters.Add
            | Rem(ValidFlags.Arithmetic flags, RegisterType vtype, Register x, Register y) ->
                if isFlagSet ArithmeticFlags.ThrowOnDivideByZero flags
                then RegisterArithmetic.Checked.rem vtype &x &y
                else RegisterArithmetic.rem vtype &x &y
                |> control.TemporaryRegisters.Add
            | Incr(ValidFlags.Arithmetic flags, RegisterType vtype, Register register) ->
                if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                then RegisterArithmetic.Checked.incr vtype &register
                else RegisterArithmetic.incr vtype &register
                |> control.TemporaryRegisters.Add
            | Decr(ValidFlags.Arithmetic flags, RegisterType vtype, Register register) ->
                if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                then RegisterArithmetic.Checked.decr vtype &register
                else RegisterArithmetic.decr vtype &register
                |> control.TemporaryRegisters.Add
            // TODO: Update type annotation for bitwise instructions to be a full TypeSignatureIndex to allow usage of pointer types.
            | And(vtype, Register x, Register y) ->
                control.TemporaryRegisters.Add(RegisterArithmetic.``and`` vtype &x &y)
            | Or(vtype, Register x, Register y) ->
                control.TemporaryRegisters.Add(RegisterArithmetic.``or`` vtype &x &y)
            | Xor(vtype, Register x, Register y) ->
                control.TemporaryRegisters.Add(RegisterArithmetic.xor vtype &x &y)
            | Not(vtype, Register register) ->
                control.TemporaryRegisters.Add(RegisterArithmetic.``not`` vtype &register)
            | Rotl(rtype, Register value, Register amount) ->
                control.TemporaryRegisters.Add(RegisterArithmetic.rotl rtype &value (ConvertRegister.s32 &amount))
            | Rotr(rtype, Register value, Register amount) ->
                control.TemporaryRegisters.Add(RegisterArithmetic.rotr rtype &value (ConvertRegister.s32 &amount))
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

            | Call(ValidFlags.Call CallFlags.DefaultValidMask flags, Method method, LookupRegisterArray arguments) ->
                invoke flags arguments method |> ignore
            | Call_virt(ValidFlags.Call CallFlags.VirtualValidMask flags, Method method, Register this, LookupRegisterArray arguments) ->
                match this.Type with
                | RegisterType.Object ->
                    let o = InterpretRegister.value<ObjectReference> &this
                    if o.IsNull then
                        if not(isFlagSet CallFlags.ThrowOnNullThis flags) then
                            invalidOp "Calling virtual method without checking for null object reference is not supported"
                        else
                            invalidOp "Cannot call virtual method with a null reference, cannot deduce the type and method to call"
                    else
                        match objectTypeLookup(gc.TypeOf o) with
                        | rm, AnyType.ReferenceType(ReferenceType.Defined idef) ->
                            let vtable = rm.TypeAt(idef).VTable
                            invoke flags (insertThisArgument arguments o) vtable.[method] |> ignore
                        | _, AnyType.ReferenceType(ReferenceType.Vector _) ->
                            invalidOp "Cannot call virtual method with an array object reference"
                        | _, bad ->
                            invalidOp(sprintf "Invalid 'this' type %A" bad)
                | RegisterType.Pointer _ ->
                    invalidOp "Cannot use pointer when calling a virtual method, since the type containing the method to call cannot be deduced"
                | RegisterType.Primitive _ ->
                    invalidOp "Cannot use primitive value as the 'this' argument when calling a virtual method"
            | Ret(LookupRegisterArray results) ->
                if results.Length < control.ReturnRegisters.Length then
                    sprintf "Expected to return %i values but only returned %i values"
                        control.ReturnRegisters.Length
                        results.Length
                    |> invalidOp

                results.AsSpan().CopyTo(Span control.ReturnRegisters) // TODO: Maybe Register could be a reference type? Ensuring registers are copied correctly may be confusing.

                returnBackControl control control.Previous results
            | Br target -> branchToTarget target
            | Br_eq(Register x, Register y, ttrue, tfalse)
            | Br_ne(Register x, Register y, tfalse, ttrue) ->
                branchToTarget (if RegisterComparison.isEqual &x &y then ttrue else tfalse)
            | Br_lt(Register x, Register y, tfalse, ttrue) ->
                branchToTarget (if RegisterComparison.isLessThan &x &y then ttrue else tfalse)
            | Br_gt(Register x, Register y, tfalse, ttrue) ->
                branchToTarget (if RegisterComparison.isGreaterThan &x &y then ttrue else tfalse)
            | Br_le(Register x, Register y, tfalse, ttrue) ->
                branchToTarget (if RegisterComparison.isLessOrEqual &x &y then ttrue else tfalse)
            | Br_ge(Register x, Register y, tfalse, ttrue) ->
                branchToTarget (if RegisterComparison.isGreaterOrEqual &x &y then ttrue else tfalse)
            | Br_true(Register condition, ttrue, tfalse) ->
                branchToTarget (if RegisterComparison.isTrueValue &condition then ttrue else tfalse)
            | Obj_new(Method constructor, LookupRegisterArray arguments) ->
                let o =
                    // TODO: Cache these AnyType instances used by obj.new
                    let typei = constructor.DeclaringType.Index
                    let rtype = AnyType.ReferenceType(ReferenceType.Defined typei)
                    let vtype = AnyType.ValueType(ValueType.Defined typei)
                    gc.Allocate (
                        garbageCollectionState,
                        objectTypeResolver constructor.DeclaringModule rtype,
                        typeSizeResolver constructor.DeclaringModule vtype
                    )

                control.TemporaryRegisters.Add(Register.ofValue RegisterType.Object o)
                roots.RootTemporary(AllocatedRegister(control.TemporaryRegisters, control.TemporaryRegisters.Count - 1))
                invoke CallFlags.None (insertThisArgument arguments o) constructor |> ignore
                // TODO: Check that calling constructor does not return any values (ensure no new temps are added), since the object above has already been added.
            | Obj_fd_ld(Field field, Register object) ->
                // TODO: If field contains a struct, perform struct copying
                let source = ObjectField.access gc objectTypeLookup typeLayoutResolver typeSizeResolver &object field
                let mutable destination =
                    Register.ofRegisterType (anyTypeToRegisterType typeSizeResolver field.DeclaringModule field.FieldType)
                source.CopyTo(Span(Unsafe.AsPointer &destination.Value, sizeof<RegisterValue>))
                control.TemporaryRegisters.Add destination
                roots.RootTemporary(AllocatedRegister(control.TemporaryRegisters, control.TemporaryRegisters.Count - 1))
            | Obj_fd_st(Field field, Register object, Register source) ->
                ObjectField.checkCanMutate field control
                // TODO: If field contains a struct, source should be an address, so perform struct copying.
                let destination = ObjectField.access gc objectTypeLookup typeLayoutResolver typeSizeResolver &object field
                Span(Unsafe.AsPointer(&Unsafe.AsRef &source.Value), destination.Length).CopyTo destination
            | Obj_fd_addr(ValidFlags.MemoryAccess MemoryAccessFlags.ElementAccessValidMask _, Field field, Register object) ->
                // TODO: Prevent throwing of exception for when field does not contain object in obj.fd.addr
                let o = InterpretRegister.value<ObjectReference> &object
                let layout = TypeLayout.ofObjectReference gc objectTypeLookup typeLayoutResolver o
                let source = ObjectField.address layout (ObjectReference.toVoidPtr o) field
                let size = uint32(typeSizeResolver field.DeclaringModule field.FieldType)
                control.TemporaryRegisters.Add(Register.ofValue (RegisterType.Pointer size) (NativePtr.ofVoidPtr<byte> source))
            | Obj_arr_new(TypeSignature etype, Register length) ->
                let array =
                    ArrayObject.allocate
                        control.CurrentModule
                        gc
                        garbageCollectionState
                        typeSizeResolver
                        objectTypeResolver
                        (anyTypeToRefOrValType etype)
                        (ConvertRegister.s32 &length)

                control.TemporaryRegisters.Add(Register.ofValue RegisterType.Object array)
                roots.RootTemporary(AllocatedRegister(control.TemporaryRegisters, control.TemporaryRegisters.Count - 1))
            | Obj_arr_get(Register array, Register index) ->
                let o = InterpretRegister.value<ObjectReference> &array
                control.TemporaryRegisters.Add(ArrayObject.get gc objectTypeLookup typeSizeResolver o (ConvertRegister.s32 &index))
            | Obj_arr_set(Register array, Register index, Register source) ->
                let o = InterpretRegister.value<ObjectReference> &array
                ArrayObject.set gc objectTypeLookup typeSizeResolver o &source (ConvertRegister.s32 &index)
            | Obj_arr_len(ValidFlags.Arithmetic flags, ty, Register array) ->
                let length = ArrayObject.length(InterpretRegister.value<ObjectReference> &array)
                if isFlagSet ArithmeticFlags.ThrowOnOverflow flags
                then StoreConstant.Checked.integer ty length
                else StoreConstant.integer ty length
                |> control.TemporaryRegisters.Add
            | Obj_arr_addr(ValidFlags.MemoryAccess MemoryAccessFlags.ElementAccessValidMask flags, Register array, Register index) ->
                let o = InterpretRegister.value<ObjectReference> &array
                let i = ConvertRegister.s32 &index

                if isFlagSet MemoryAccessFlags.ThrowOnInvalidAccess flags && i >= ArrayObject.length o then
                    raise(IndexOutOfRangeException(sprintf "Attempt to access array element out of bounds for %O" array))

                let mutable struct(elemm, elemt) as etype = Unchecked.defaultof<_>
                let address = ArrayObject.item gc objectTypeLookup typeSizeResolver o i &etype
                let esize = uint32(typeSizeResolver elemm elemt)
                control.TemporaryRegisters.Add(Register.ofValue (RegisterType.Pointer esize) (NativePtr.ofVoidPtr<byte> address))
            | Obj_arr_const(TypeSignature etype, Data data) ->
                let esize = typeSizeResolver control.CurrentModule etype
#if DEBUG
                if data.Length % esize <> 0 then
                    raise(NotImplementedException "TODO: How to handle mismatch in constant data array length?")
#endif
                let array =
                    ArrayObject.allocate
                        control.CurrentModule
                        gc
                        garbageCollectionState
                        typeSizeResolver
                        objectTypeResolver
                        (anyTypeToRefOrValType etype)
                        (data.Length / esize)

                control.TemporaryRegisters.Add(Register.ofValue RegisterType.Object array)
                roots.RootTemporary(AllocatedRegister(control.TemporaryRegisters, control.TemporaryRegisters.Count - 1))
                // NOTE: Creation of array from constant data might not work when endianness of data and runtime is different.
                data.AsSpan().CopyTo(Span(ArrayObject.address array, data.Length))
            | Mem_st(ValidFlags.MemoryAccess MemoryAccessFlags.RawAccessValidMask _, Register src, TypeSignature ty, Register addr) ->
                // TODO: Throw exception on invalid memory store.
                let address = InterpretRegister.value<nativeptr<byte>> &addr
                MemoryOperations.store typeSizeResolver (NativePtr.toVoidPtr address) &src control.CurrentModule ty
            | Mem_ld(ValidFlags.MemoryAccess MemoryAccessFlags.RawAccessValidMask _, TypeSignature ty, Register addr) ->
                // TODO: If type is a struct, source is a register containing an address, so perform struct copying when loading from memory.
                // TODO: Throw exception on invalid memory load.
                let address = InterpretRegister.value<nativeptr<byte>> &addr
                control.TemporaryRegisters.Add(MemoryOperations.load stack typeSizeResolver (NativePtr.toVoidPtr address) control.CurrentModule ty)
            | Alloca(Register count, TypeSignature ty) ->
                // TODO: Have flag to skip 0 init.
                let esize = typeSizeResolver control.CurrentModule ty
                let count = ConvertRegister.s32 &count
                let address = MemoryOperations.alloca stack esize count

                if count > 0 then
                    match ty with
                    | AnyType.ReferenceType _ ->
                        let start = StackPtr.ofVoidPtr<ObjectReference> address
                        for i = 0 to count - 1 do roots.RootAddress(StackPtr.add start i)
                    | AnyType.ValueType(ValueType.Defined typei) ->
                        let layout = typeLayoutResolver(control.CurrentModule.TypeAt typei).References
                        if not layout.IsDefaultOrEmpty then
                            for elemi = 0 to count - 1 do
                                let start = StackPtr.add (StackPtr.ofVoidPtr<byte> address) elemi
                                for offseti = 0 to layout.Length - 1 do
                                    StackPtr.add start offseti
                                    |> StackPtr.reinterpret<_, ObjectReference>
                                    |> roots.RootAddress
                    | AnyType.ValueType(ValueType.Primitive _ | ValueType.UnsafePointer _) -> ()
                    | AnyType.SafePointer _ ->
                        raise(NotImplementedException "TODO: Add roots when alloca is used with a safe pointer")

                NativePtr.ofVoidPtr<byte> address
                |> Register.ofValue (RegisterType.Pointer(uint32 esize))
                |> control.TemporaryRegisters.Add
            | Mem_init(ValidFlags.MemoryAccess MemoryAccessFlags.RawAccessValidMask _, Register count, TypeSignature ty, Register address, Register value) ->
                // TODO: Could be more efficient with mem.init, don't need to calculate size and other things with each store.
                let start = InterpretRegister.value<nativeptr<byte>> &address
                let size = typeSizeResolver control.CurrentModule ty
                for i = 0 to ConvertRegister.s32 &count do
                    let address = NativePtr.add start (i * size)
                    MemoryOperations.store typeSizeResolver (NativePtr.toVoidPtr address) &value control.CurrentModule ty
            | Mem_init_const(ValidFlags.MemoryAccess MemoryAccessFlags.RawAccessValidMask _, TypeSignature ty, Register address, Data data) ->
#if DEBUG // NOTE: For mem.init.const, type signature might not be needed a simple copying of bytes is performed. Could be kept in order to check that the data.Length makes sense.
                let esize = typeSizeResolver control.CurrentModule ty
                if data.Length % esize <> 0 then // Duplicated from obj.arr.const
                    raise(NotImplementedException "TODO: How to handle mismatch in constant data array length?")
#endif
                let destination = NativePtr.toVoidPtr(InterpretRegister.value<nativeptr<byte>> &address)
                data.AsSpan().CopyTo(Span<byte>(destination, data.Length))
            | Mem_cpy(ValidFlags.MemoryAccess MemoryAccessFlags.RawAccessValidMask _, Register count, TypeSignature ty, Register source, Register destination) ->
                let esize = typeSizeResolver control.CurrentModule ty
                let src = InterpretRegister.value<nativeptr<byte>> &source
                let dest = InterpretRegister.value<nativeptr<byte>> &destination
                let length = esize * ConvertRegister.s32 &count
                Span<byte>(NativePtr.toVoidPtr src, length).CopyTo(Span<byte>(NativePtr.toVoidPtr dest, length))
            | Obj_throw(Register ex) ->
                caughtProgramException <- ValueSome { ExceptionRegister.Value = ex; ExceptionRegister.Origin = control }
            | Nop -> ()

            match runExternalCode with
            | ValueNone -> ()
            | ValueSome run ->
                let runtimeStackFrame = frame.Value.Value
                try
                    run.Invoke (
                        gc,
                        garbageCollectionState,
                        typeSizeResolver,
                        objectTypeResolver,
                        objectTypeLookup,
                        typeLayoutResolver,
                        runtimeStackFrame
                    )

                    // TODO: Handle return values from internal call.
                    returnBackControl runtimeStackFrame runtimeStackFrame.Previous ImmutableArray.Empty
                finally
                    runExternalCode <- ValueNone

            // Incrementing here means index points to instruction that caused the exception in the stack frame.
            control.InstructionIndex <- Checked.(+) control.InstructionIndex 1
        with
        | ex ->
            let message = sprintf "Internal interpreter error: %s%s" Environment.NewLine control.StackTrace
            raise(InvalidOperationException(message, ex))

        if caughtProgramException.IsSome then
            while frame.Value.IsSome && caughtProgramException.IsSome do
                let currentHandlerFrame = frame.Value.Value
                match currentHandlerFrame.CurrentExceptionHandler with
                | ValueSome({ BlockExceptionHandler.CatchBlock = Index handlerBlockIndex } as handler) ->
                    let exr = AllocatedRegister(Array.singleton caughtProgramException.Value.Value, 0)
                    branchToTarget(int32 handlerBlockIndex - currentHandlerFrame.BlockIndex)
                    garbageCollectionState.Roots.RootTemporary exr
                    control.InstructionIndex <- 0

                    match handler.ExceptionRegister with
                    | ValueSome eindex ->
                        currentHandlerFrame.AddExceptionRegister(eindex, exr)
                        caughtProgramException <- ValueNone
                    | ValueNone -> ()
                | ValueNone ->
                    returnBackControl currentHandlerFrame currentHandlerFrame.Previous ImmutableArray.Empty

    match caughtProgramException with
    | ValueSome(exr: ExceptionRegister) ->
        let message =
            sprintf "Uncaught program exception: %s%s%s" (exr.Value.ToString()) Environment.NewLine exr.Origin.StackTrace

        raise(UncaughtProgramException(message, exr))
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

let objectTypeResolver(): struct(ObjectTypeResolver * ObjectTypeLookup) =
    let objectTypeLookup = Dictionary<struct(ResolvedModule * AnyType), ObjectType>()
    let reverseTypeLookup = Dictionary<ObjectType, _>()
    let tresolver owner ty =
        let key = struct(owner, ty)
        match objectTypeLookup.TryGetValue key with
        | true, existing -> existing
        | false, _ ->
            let i = ObjectType(uint32 objectTypeLookup.Count)
            reverseTypeLookup.Add(i, key)
            objectTypeLookup.Add(key, i)
            i
    struct(tresolver, fun ty -> reverseTypeLookup.[ty])

let typeSizeResolver (typeLayoutResolver: TypeLayoutResolver) (objectTypeResolver: ObjectTypeResolver): TypeSizeResolver =
    let lookup = Dictionary<ObjectType, int32>()
    fun owner ty ->
        let i = objectTypeResolver owner ty
        match lookup.TryGetValue i with
        | true, size -> size
        | false, _ ->
            let size =
                match ty with
                | AnyType.ValueType(ValueType.Primitive(PrimitiveType.Bool | PrimitiveType.S8 | PrimitiveType.U8)) -> 1
                | AnyType.ValueType(ValueType.Primitive(PrimitiveType.S16 | PrimitiveType.U16 | PrimitiveType.Char16)) -> 2
                | AnyType.ValueType(ValueType.Primitive(PrimitiveType.S32 | PrimitiveType.U32 | PrimitiveType.Char32 | PrimitiveType.F32)) -> 4
                | AnyType.ValueType(ValueType.Primitive(PrimitiveType.S64 | PrimitiveType.U64 | PrimitiveType.F64)) -> 8
                | AnyType.ValueType(ValueType.Primitive(PrimitiveType.SNative | PrimitiveType.UNative))
                | AnyType.ValueType(ValueType.UnsafePointer _)
                | AnyType.SafePointer _ -> sizeof<nativeint>
                | AnyType.ValueType(ValueType.Defined tindex) -> typeLayoutResolver(owner.TypeAt tindex).Size
                | AnyType.ReferenceType _ -> sizeof<ObjectReference>
            lookup.Add(i, size)
            size

let typeLayoutResolver (typeSizeResolver: TypeSizeResolver): TypeLayoutResolver =
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
                let fsize = typeSizeResolver ty.DeclaringModule defined.FieldType

                fields.[defined] <- size

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
    let struct(tresolver, tlookup) = objectTypeResolver()
    let sizes = ref Unchecked.defaultof<TypeSizeResolver>
    let layouts = typeLayoutResolver(fun md ty -> sizes.Value md ty)
    do sizes.Value <- typeSizeResolver layouts tresolver

    static member Initialize
        (
            program,
            ?moduleImportLoader,
            ?garbageCollectorStrategy
        )
        =
        new Runtime (
            program,
            defaultArg moduleImportLoader (fun _ -> ValueNone),
            defaultArg garbageCollectorStrategy (GarbageCollectors.MarkAndSweep())
        )

    member _.Program = program

    member private _.AllocateArray(garbageCollectionState, etype, length) =
        ArrayObject.allocate program garbageCollectorStrategy garbageCollectionState sizes.Value tresolver etype length

    member runtime.InvokeEntryPoint(argv: string[], ?maxStackCapacity, ?interpreterEventHandler, ?stackEventHandler) =
        match program.EntryPoint with
        | ValueSome main ->
            if main.DeclaringModule <> program then
                raise(MissingEntryPointException "The entry point method for a module must be defined in the module")

            let garbageCollectionState = GarbageCollectionState(sizes.Value, tlookup, layouts)

            let arguments =
                let parameters = main.Signature.ParameterTypes
                match parameters.Length with
                | 0 -> ImmutableArray.Empty
                | 1 ->
                    match program.TypeSignatureAt parameters.[0] with
                    | AnyType.ReferenceType(ReferenceType.Vector(ReferenceOrValueType.Reference(ReferenceType.Vector tchar) as tstr)) ->
                        let argv' = runtime.AllocateArray(garbageCollectionState, tstr, argv.Length)
                        //garbageCollectorStrategy.Roots.Add argv'

                        match tchar with
                        | ReferenceOrValueType.Value(ValueType.Primitive PrimitiveType.Char32) ->
                            let start = NativePtr.ofVoidPtr<ObjectReference>(ArrayObject.address argv')
                            let buffer = List<_>()
                            for i = 0 to argv.Length - 1 do
                                buffer.Clear()
                                buffer.AddRange(argv.[i].EnumerateRunes())

                                let argument = runtime.AllocateArray(garbageCollectionState, tchar, buffer.Count)
                                //garbageCollectorStrategy.Roots.Add argument
                                NativePtr.write (NativePtr.add start i) argument

                                let chars = NativePtr.ofVoidPtr<System.Text.Rune>(ArrayObject.address argument)
                                for chari = 0 to buffer.Count - 1 do
                                    NativePtr.write (NativePtr.add chars chari) buffer.[chari]
                        | _ -> invalidOp(sprintf "%A is not a valid character type" tchar)

                        ImmutableArray.Create(Register.ofValue RegisterType.Object argv')
                    | bad -> invalidOp(sprintf "%A is not a valid argument for the entry point" bad)
                | i -> invalidOp(sprintf "Invalid signature for entry point, cannot have %i arguments" i)

            if main.Signature.ReturnTypes.Length > 1 then
                failwith "TODO: Error for multiple return values are not supported in entry point"

            let results =
                interpret
                    garbageCollectorStrategy
                    garbageCollectionState
                    (defaultArg maxStackCapacity DefaultStackCapacity)
                    sizes.Value
                    tresolver
                    tlookup
                    layouts
                    interpreterEventHandler
                    stackEventHandler
                    arguments
                    main

            if Array.isEmpty results then 0 else ConvertRegister.s32 &results.[0]
        | ValueNone ->
            raise(MissingEntryPointException "The entry point method of the module is not defined")

    interface IDisposable with member _.Dispose() = (garbageCollectorStrategy :> IDisposable).Dispose()
