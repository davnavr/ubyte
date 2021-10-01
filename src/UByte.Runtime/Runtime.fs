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
    member _.InstructionIndex = iindex
    member _.LocalRegisters = locals
    member _.ReturnRegisters = returns
    member _.Instructions = instructions
    member _.CurrentMethod = method
    member _.Previous = prev

    member this.RegisterAt (Index i: RegisterIndex) =
        let i' = Checked.int32 i
        if i' >= args.Length then this.LocalRegisters.[i' - args.Length] else args.[i']

    member _.IncrementInstructionIndex() = iindex <- Checked.(+) 1 iindex

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

let inline (|RuntimeObjectFields|) (o: RuntimeObject): RuntimeStruct = o.Fields

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type RuntimeStruct =
    { RawData: byte[]
      References: RuntimeObject[] }

    member this.ReadRaw<'T when 'T : struct and 'T :> ValueType and 'T : (new: unit -> 'T)> index: 'T =
        MemoryMarshal.Read<'T>(ReadOnlySpan(this.RawData).Slice(index))

    member this.WriteRaw<'T when 'T : struct and 'T :> ValueType and 'T : (new: unit -> 'T)>(index, value: 'T) =
        let mutable value = value
        MemoryMarshal.Write<'T>(Span(this.RawData).Slice(index), &value)

[<RequireQualifiedAccess>]
module Interpreter =
    open UByte.Format.Model.InstructionSet

    let private copyRegisterValues (source: ImmutableArray<RuntimeRegister>) (dest: ImmutableArray<RuntimeRegister>) =
        if source.Length > dest.Length then failwith "TODO: Error, more source registers than destination registers"
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

    [<RequireQualifiedAccess>]
    module private Arithmetic =
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
        let sub xreg yreg rreg = binaryOp (-) (-) (-) (-) (-) yreg xreg rreg // Reversed since x must be subtracted from y

        let ``and`` xreg yreg rreg = binaryOp (&&&) (&&&) (&&&) (&&&) (&&&) xreg yreg rreg
        let ``or`` xreg yreg rreg = binaryOp (|||) (|||) (|||) (|||) (|||) xreg yreg rreg
        //let ``not`` xreg yreg rreg = unaryOp (~~~) (~~~) (~~~) (~~~) (~~~) xreg yreg rreg
        let xor xreg yreg rreg = binaryOp (^^^) (^^^) (^^^) (^^^) (^^^) xreg yreg rreg

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

    let private (|Registers|) (frame: RuntimeStackFrame) (registers: ImmutableArray<RegisterIndex>) =
        let mutable registers' = Array.zeroCreate registers.Length
        for i = 0 to registers.Length - 1 do registers'.[i] <- frame.RegisterAt registers.[i]
        Unsafe.As<RuntimeRegister[], ImmutableArray<RuntimeRegister>> &registers'

    let interpret returns arguments (entrypoint: RuntimeMethod) =
        let mutable frame: RuntimeStackFrame voption = ValueNone

        let inline invoke returns arguments (method: RuntimeMethod) =
            method.SetupStackFrame(returns, &frame)
            copyRegisterValues arguments frame.Value.ArgumentRegisters

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

            let inline fieldAccessInstruction field object register execute =
                match object with
                | RuntimeRegister.RRef { contents = null } -> nullReferenceFieldAccess frame' field
                | RuntimeRegister.RRef { contents = RuntimeObjectFields data }
                | RuntimeRegister.RStruct data -> execute data
                | _ -> numericFieldAccess frame' field

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
                | And(Register x, Register y, Register r) -> Arithmetic.``and`` x y r
                | Or(Register x, Register y, Register r) -> Arithmetic.``or`` x y r
                | Xor(Register x, Register y, Register r) -> Arithmetic.xor x y r
                | Const_i32(value, Register dest) -> Const.i32 value dest
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
                | Obj_null(Register destination) ->
                    match destination with
                    | RuntimeRegister.RRef destination' -> destination'.contents <- null
                    | RuntimeRegister.RNative destination' -> destination'.contents <- 0un
                    | _ -> raise(RuntimeException(frame, "Unable to store null reference into register"))
                | Obj_new(Method constructor, Registers frame' arguments, Register destination) ->
                    match destination with
                    | RuntimeRegister.RRef o ->
                        o.contents <- new RuntimeObject(constructor.DeclaringType)
                        // TODO: Check that first argument is an object reference.
                        invoke ImmutableArray.Empty (arguments.Insert(0, destination)) constructor
                    | bad ->
                        failwithf "TODO: Error for cannot store object reference here after calling constructor %A" bad
                | Obj_ldfd(Field field, Register object, Register destination) ->
                    field.CheckMutate frame'

                    fieldAccessInstruction field object destination <| fun fields ->
                        match destination with // TODO: How to respect endianness when reading values from struct
                        | RuntimeRegister.R1 destination' ->
                            destination'.contents <- fields.RawData.[field.Offset]
                        | RuntimeRegister.R2 destination' ->
                            destination'.contents <- BitConverter.ToUInt16(fields.RawData, field.Offset)
                        | RuntimeRegister.R4 destination' ->
                            destination'.contents <- BitConverter.ToUInt32(fields.RawData, field.Offset)
                        | RuntimeRegister.R8 destination' ->
                            destination'.contents <- BitConverter.ToUInt64(fields.RawData, field.Offset)
                        | RuntimeRegister.RNative destination' ->
                            destination'.contents <- fields.ReadRaw<unativeint> field.Offset
                        | RuntimeRegister.RStruct _ ->
                            failwith "// TODO: How to read structs stored inside other structs?"
                        | RuntimeRegister.RRef destination' ->
                            destination'.contents <- fields.References.[field.Offset]
                | Obj_stfd(Field field, Register object, Register source) ->
                    fieldAccessInstruction field object source <| fun fields ->
                        match source with
                        | RuntimeRegister.R1 { contents = value } -> fields.RawData.[field.Offset] <- value
                        | RuntimeRegister.R2 { contents = value } -> fields.WriteRaw(field.Offset, value)
                        | RuntimeRegister.R4 { contents = value } -> fields.WriteRaw(field.Offset, value)
                        | RuntimeRegister.R8 { contents = value } -> fields.WriteRaw(field.Offset, value)
                        | RuntimeRegister.RNative { contents = value } -> fields.WriteRaw(field.Offset, value)
                        | RuntimeRegister.RStruct _ ->
                            failwith "// TODO: How to store structs stored inside other structs?"
                        | RuntimeRegister.RRef { contents = value } -> fields.References.[field.Offset] <- value
                | Nop -> ()
                | bad -> failwithf "TODO: Unsupported instruction %A" bad

                frame'.IncrementInstructionIndex()
            with
            | e ->
                //ex <- ValueSome e
                raise(System.NotImplementedException("TODO: Implement exception handling, " + e.Message, e))

            ()

        match frame with
        | ValueNone -> ()
        | ValueSome bad ->
            failwith "TODO: Error for method did not have Ret instruction"

[<Sealed; AllowNullLiteral>]
type RuntimeObject (otype: RuntimeTypeDefinition) =
    member val Fields = otype.InitializeObjectFields()

    member _.SetField(field: RuntimeField) =
        failwith "TODO: Call helper to calculate where the field value is stored"

[<Sealed>]
type InvalidConstructorException (method: RuntimeMethod, frame, message) =
    inherit RuntimeException(frame, message)

    member _.Method = method

[<Sealed>]
type RuntimeMethod (rmodule: RuntimeModule, method: Method) =
    let { Method.MethodName = name; MethodFlags = flags; Body = body } = method

    member _.Module: RuntimeModule = rmodule

    member val Name = rmodule.IdentifierAt name

    member val DeclaringType = rmodule.InitializeType method.MethodOwner

    member _.IsInstance = isFlagSet MethodFlags.Instance flags

    member _.IsConstructor = isFlagSet MethodFlags.ConstructorMask flags

    member private _.CreateRegister rtype =
        match rmodule.TypeSignatureAt rtype with
        | AnyType.Primitive prim ->
            match prim with
            | PrimitiveType.Bool
            | PrimitiveType.S8
            | PrimitiveType.U8 -> fun() -> RuntimeRegister.R1(ref 0uy)
            | PrimitiveType.S16
            | PrimitiveType.U16
            | PrimitiveType.Char16 -> fun() -> RuntimeRegister.R2(ref 0us)
            | PrimitiveType.S32
            | PrimitiveType.U32
            | PrimitiveType.F32
            | PrimitiveType.Char32 -> fun() -> RuntimeRegister.R4(ref 0u)
            | PrimitiveType.S64
            | PrimitiveType.U64
            | PrimitiveType.F64 -> fun() -> RuntimeRegister.R8(ref 0UL)
            | PrimitiveType.SNative
            | PrimitiveType.UNative -> fun() -> RuntimeRegister.RNative(ref 0un)
            | PrimitiveType.Unit -> fun() -> failwith "TODO: Prevent usage of Unit in register types."
        | AnyType.ObjectReference _ -> fun() -> RuntimeRegister.RRef(ref null)
        | bad -> failwithf "TODO: Unsupported type for register %A" bad

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

    let offset =
        let mutable offset' = 0
        lazy 0

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
                    indices.Add(rfield, max dsize rlen)

                    sumDataSize <- sumDataSize + dsize
                    sumReferencesLength <- sumReferencesLength + dsize

                { RuntimeTypeLayout.Fields = fields'
                  FieldIndices = indices
                  RawDataSize = sumDataSize
                  ObjectReferencesLength = sumReferencesLength }
        else emptyTypeLayout

    //let globals: RuntimeStruct

    // TODO: Cache length of RawData and References arrays for RuntimeObject and RuntimeStruct

    member _.Module = rm

    member _.Layout: RuntimeTypeLayout = layout.Value

    member val Name = rm.IdentifierAt t.TypeName

    member _.InitializeObjectFields(): RuntimeStruct =
        let layout' = layout.Value
        { RuntimeStruct.RawData = Array.zeroCreate layout'.RawDataSize
          References = Array.zeroCreate layout'.ObjectReferencesLength }

let createIndexedLookup (count: int32) initializer =
    let lookup = Dictionary<Index<_>, _> count
    fun i ->
        match lookup.TryGetValue i with
        | true, existing -> existing
        | false, _ ->
            let value = initializer i
            lookup.Add(i, value)
            value

[<Sealed>]
type MissingEntryPointException (m: RuntimeModule, message: string) =
    inherit RuntimeException(ValueNone, message)

    member _.Module = m

//type State // TODO: Keep track of format version supported by the runtime.

[<Sealed>]
type RuntimeModule (m: Module, moduleImportResolver: ModuleIdentifier -> RuntimeModule) as rm =
    // TODO: Check if the runtime version matches the module's format version.

    // TODO: Account for imported type defs when resolving indices.
    let definedTypeLookup =
        let types = m.Definitions.DefinedTypes
        createIndexedLookup types.Length <| fun (Index i) -> RuntimeTypeDefinition(rm, types.[Checked.int32 i])

    // TODO: Account for imported methods when resolving indices.
    let definedMethodLookup =
        let methods = m.Definitions.DefinedMethods
        createIndexedLookup methods.Length <| fun (Index i) -> RuntimeMethod(rm, methods.[Checked.int32 i])

    // TODO: Account for imported fields when resolving indices.
    let definedFieldLookup =
        let owners = m.Definitions.DefinedTypes
        let fields = m.Definitions.DefinedFields
        createIndexedLookup fields.Length <| fun (Index i as i') ->
            let f = fields.[Checked.int32 i]
            let (Index owner) = f.FieldOwner
            let n = owners.[Checked.int32 owner].Fields.IndexOf i'
            RuntimeField(rm, f, n)

    member _.IdentifierAt(Index i: IdentifierIndex) = m.Identifiers.Identifiers.[Checked.int32 i]

    member _.TypeSignatureAt(Index i: TypeSignatureIndex) = m.TypeSignatures.[Checked.int32 i]

    member _.MethodSignatureAt(Index i: MethodSignatureIndex) = m.MethodSignatures.[Checked.int32 i]

    member _.CodeAt(Index i: CodeIndex) = m.Code.[Checked.int32 i]

    member _.InitializeMethod i = definedMethodLookup i

    member _.InitializeField i = definedFieldLookup i

    member _.InitializeType i = definedTypeLookup i

    member _.ComputeFieldSize(index, rawDataSize: outref<_>, objectReferencesLength: outref<_>) =
        let field = definedFieldLookup index

        match field.FieldType with
        | Primitive PrimitiveType.S32 -> rawDataSize <- 4
        | ObjectReference _ -> objectReferencesLength <- 1
        | bad -> failwithf "TODO: Unsupported field type %A" bad

        field

    member this.InvokeEntryPoint(argv: string[]) =
        match m.EntryPoint with
        | ValueSome ei ->
            let main = this.InitializeMethod ei
            let arguments = ImmutableArray.Empty // TODO: Check signature of method to determine if argv should be passed.
            let result = ref 0u // TODO: Check that entry point does not return more than 1 value.

            Interpreter.interpret (ImmutableArray.Create(RuntimeRegister.R4 result)) arguments main
            int32 result.contents
        | ValueNone -> raise(MissingEntryPointException(this, "The entry point method of the module is not defined"))

let initialize program moduleImportLoader =
    /// A cache for the modules created by the import loader.
    let moduleImportResolver =
        let resolved = Dictionary<ModuleIdentifier, RuntimeModule> program.Imports.ImportedModules.Length
        let rec resolver import =
            match resolved.TryGetValue import with
            | true, existing -> existing
            | false, _ ->
                let r = RuntimeModule(moduleImportLoader import, resolver)
                resolved.Add(import, r)
                r
        resolver

    RuntimeModule(program, moduleImportResolver)
