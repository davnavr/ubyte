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

[<RequireQualifiedAccess>]
module Register =
    let ofRegisterType rtype = { Register.Value = Unchecked.defaultof<uint64>; Register.Type = rtype }

    let ofTypeIndex (rm: ResolvedModule) typei =
        match rm.TypeSignatureAt typei with
        | AnyType.ValueType(ValueType.Primitive prim) ->
            RegisterType.Primitive prim
        | AnyType.ReferenceType _ ->
            RegisterType.Object
        | AnyType.SafePointer _ | AnyType.ValueType(ValueType.Defined _ | ValueType.UnsafePointer _) ->
            RegisterType.Pointer
        |> ofRegisterType

let [<Literal>] private MaxStackCapacity = 0xFFFFF

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
        let frame' = StackFrame(frame.Value, args, 0u, returns, ImmutableArray.Empty, method)
        frame.contents <- ValueSome frame'
        runExternalCode <- ValueSome <| fun (frame'': _ voption ref) ->
            ExternalCode.call library' efunction' frame''.Value.Value
            frame''.contents <- frame'.Previous

let private interpret
    (gc: IGarbageCollector)
    (arguments: ImmutableArray<Register>)
    (entrypoint: ResolvedMethod)
    =
    let mutable frame: StackFrame voption ref = ref ValueNone
    let mutable runExternalCode: (StackFrame voption ref -> unit) voption = ValueNone
    let mutable ex = ValueNone
    use stack = new ValueStack(MaxStackCapacity)

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
        | ValueSome _ -> raise(NotImplementedException "TODO: Reimplement exception handling, with support for moving back up the call stack")
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
        //let inline (|RegisterType|) (TypeSignature rtype) = anyTypeToRegisterType rtype
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
            | Ret(LookupRegisterArray results) ->
                if results.Length < control.ReturnRegisters.Length then
                    sprintf "Expected to return %i values but only returned %i values"
                        control.ReturnRegisters.Length
                        results.Length
                    |> invalidOp

                Span(results).CopyTo(Span control.ReturnRegisters)
                frame.Value <- control.Previous
                stack.FreeAllocations()
            | Nop -> ()

            match runExternalCode with
            | ValueNone -> ()
            | ValueSome run ->
                run frame
                runExternalCode <- ValueNone // TODO: Avoid code duplication with ret.
                stack.FreeAllocations()

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

            failwith "TODO: Calculate the layout"
    inner

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

    member _.InvokeEntryPoint(argv: string[]) =
        match program.EntryPoint with
        | ValueSome main ->
            if main.DeclaringModule <> program then
                raise(MissingEntryPointException "The entry point method for a module must be defined in the module")

            let arguments =
                if main.Signature.ParameterTypes.Length = 0 then
                    ImmutableArray.Empty
                else
                    failwith "TODO: argv is not yet supported"

            if main.Signature.ReturnTypes.Length > 1 then failwith "TODO: Error for multiple return values are not supported in entry point"

            let results = interpret garbageCollectorStrategy arguments main

            if Array.isEmpty results then 0 else int32 results.[0].Value
        | ValueNone ->
            raise(MissingEntryPointException "The entry point method of the module is not defined")
