[<RequireQualifiedAccess>]
module UByte.Runtime.Interpreter

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open UByte.Format.Model
open UByte.Format.Model.InstructionSet

open UByte.Resolver
open UByte.Runtime.MemoryManagement

let inline isFlagSet flag value = value &&& flag = flag

[<Struct; NoComparison; NoEquality>]
type Register = { mutable Value: unativeint; Type: RegisterType }

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

let private setupStackFrame (method: ResolvedMethod) (frame: StackFrame voption ref) (runExternalCode: byref<_ voption>) =
    failwith "TODO: Setup stack frame"
    ()


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

[<NoComparison; NoEquality>]
type TypeLayout = { Size: int32; Fields: Dictionary<ResolvedField, int32> }

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

            if main.Signature.ParameterTypes.Length <> 0 then failwith "TODO: ARGV is not yet supported"

            failwith "BAD"
            -1
        | ValueNone ->
            raise(MissingEntryPointException "The entry point method of the module is not defined")
