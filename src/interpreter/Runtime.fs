module rec UByte.Interpreter.Runtime

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Runtime.CompilerServices

open UByte.Format
open UByte.Format.Model

[<RequireQualifiedAccess>]
type RuntimeRegister =
    | R1 of uint8 ref
    | R2 of uint16 ref
    | R4 of uint32 ref
    | R8 of uint64 ref
    //| RStruct of byte[]
    | RPtr of obj ref

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
        | R2 _, R1 _
        | R4 _, R2 _
        | R4 _, R1 _
        | R8 _, R4 _
        | R8 _, R2 _
        | R8 _, R1 _ -> failwith "TODO: Error for size mismatch"
        | RPtr { contents = value }, RPtr dest -> dest.contents <- value
        | RPtr _, _
        | _, RPtr _ -> failwith "TODO: Error for cannot mix integers and reference types"

[<Sealed>]
type RuntimeStackFrame
    (
        prev: RuntimeStackFrame voption,
        args: ImmutableArray<RuntimeRegister>,
        registers: ImmutableArray<RuntimeRegister>,
        method: RuntimeMethod voption
    )
    =
    member _.MethodArguments = args
    member _.Registers = registers
    member _.CurrentMethod = method
    member _.Previous = prev

    member this.RegisterAt (Index i: RegisterIndex) =
        let i' = Checked.int32 i
        if i' >= args.Length then this.Registers.[i' - args.Length] else args.[i']

type MethodInvocationResult = ImmutableArray<RuntimeRegister>

[<RequireQualifiedAccess>]
module Interpreter =
    open UByte.Format.Model.InstructionSet

    let copyRegisterValues (source: ImmutableArray<RuntimeRegister>) (dest: ImmutableArray<RuntimeRegister>) =
        if source.Length <> dest.Length then failwith "TODO: Error, register count mismatch"
        for i = 0 to source.Length - 1 do source.[i].CopyValueTo dest.[i]

    let rec private loop
        (current: RuntimeStackFrame)
        (instructions: ImmutableArray<_>)
        (methodIndexResolver: _ -> RuntimeMethod)
        i
        =
        if i < instructions.Length then
            let inline getIndexedRegisters (registers: ImmutableArray<RegisterIndex>) =
                let mutable registers' = Array.zeroCreate registers.Length
                for i = 0 to registers.Length - 1 do registers'.[i] <- current.RegisterAt registers.[i]
                Unsafe.As<RuntimeRegister[], ImmutableArray<RuntimeRegister>> &registers'

            match instructions.[i] with
            | Instruction.Nop -> loop current instructions methodIndexResolver (i + 1)
            | Instruction.Ret rregisters -> getIndexedRegisters rregisters
            | Instruction.Call(methodi, aregisters, rregisters) ->
                // NOTE: Can optimize by avoiding allocation of new register instances in CreateArgumentRegisters IFF no arg registers in the method are mutated
                let returned = (methodIndexResolver methodi).Invoke(current, copyRegisterValues (getIndexedRegisters aregisters))
                for i = 0 to returned.Length - 1 do returned.[i].CopyValueTo(current.RegisterAt rregisters.[i])
                loop current instructions methodIndexResolver (i + 1)
            | Instruction.Reg_copy(source, dest) ->
                (current.RegisterAt source).CopyValueTo(current.RegisterAt dest)
                loop current instructions methodIndexResolver (i + 1)
            | Instruction.Const_s32(value, dest) ->
                match current.RegisterAt dest with
                | RuntimeRegister.R4 dest' -> dest'.contents <- uint32 value
                | RuntimeRegister.R8 dest' -> dest'.contents <- uint64 value
                | _ -> failwith "TODO: Error for cannot store 32 bit integer into register"
                loop current instructions methodIndexResolver (i + 1)
        else
            failwith "TODO: error for method did not return"

    let interpret current instructions methodIndexResolver =
        //let current = ref current
        // NOTE: Can do an explicit stack of Stack<struct(RuntimeStackFrame * ImmutableArray<Instruction> * int32)>
        loop current instructions methodIndexResolver 0

[<Sealed>]
type RuntimeMethod (rmodule: RuntimeModule, method: Method) =
    member _.Module = rmodule

    member _.Name = rmodule.IdentifierAt method.MethodName

    member _.HasThis = method.MethodFlags &&& MethodFlags.Static <> MethodFlags.Static

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
            | PrimitiveType.F64 -> fun() -> RuntimeRegister.R4(ref 0u)
            | PrimitiveType.Unit -> fun() -> failwith "TODO: Prevent usage of Unit in register types."
        | _ -> failwith "TODO: Unsupported type for register"

    member this.CreateArgumentRegisters() =
        let args = rmodule.MethodSignatureAt method.Signature
        let mutable registers =
            let mutable length = args.ParameterTypes.Length
            if this.HasThis then length <- length + 1
            Array.zeroCreate length
        for i = 0 to registers.Length - 1 do
            if i = 0 && this.HasThis then failwith "TODO: Add this pointer"
            registers.[0] <- this.CreateRegister args.ParameterTypes.[i] ()
        Unsafe.As<RuntimeRegister[], ImmutableArray<RuntimeRegister>> &registers

    member this.Invoke(previous: RuntimeStackFrame, arguments: _ -> unit): MethodInvocationResult = 
        match method.Body with
        | MethodBody.Defined codei ->
            let code = rmodule.CodeAt codei

            let args = this.CreateArgumentRegisters()

            let registers =
                let registers = ImmutableArray.CreateBuilder()
                for struct(count, rtype) in code.RegisterTypes do
                    let create = this.CreateRegister rtype.RegisterType
                    for _ = 1 to Checked.int32 count do create() |> registers.Add
                registers.ToImmutable()

            let current = RuntimeStackFrame(ValueSome previous, args, registers, ValueSome this)

            arguments args

            Interpreter.interpret current code.Instructions rmodule.InitializeMethod
        | MethodBody.Abstract -> failwith "TODO: Handle virtual calls"

[<Sealed>]
type RuntimeTypeDefinition (rmodule: RuntimeModule, t: TypeDefinition, mstart: MethodIndex) =
    let (Index mstart) = mstart
    let methods = Dictionary<MethodIndex, RuntimeMethod> t.Methods.Length
    let name = lazy rmodule.IdentifierAt t.TypeName

    member _.Module = rmodule

    member _.Name = name.Value

    member _.InitializeMethod(Index i as mi) =
        match methods.TryGetValue mi with
        | true, existing -> existing
        | false, _ ->
            let method = RuntimeMethod(rmodule, t.Methods.[Checked.int32 mstart + Checked.int32 i])
            methods.Add(mi, method)
            method

[<Sealed>]
type RuntimeModule (m: Module, moduleImportResolver: ModuleImport -> RuntimeModule) as rm =
    let typeDefLookup, methodDefLookup =
        let mutable typei =
            let mutable i = 0u
            for mimport in m.Imports do
                for ns in mimport.ImportedNamespaces do i <- i + uint32 ns.TypeImports.Length + uint32 ns.TypeAliases.Length
            i

        let mutable methodi = 0u

        let types = Dictionary<TypeDefinitionIndex, RuntimeTypeDefinition>()
        let methods = Dictionary<MethodIndex, TypeDefinitionIndex>()

        for ni = 0 to m.Namespaces.Length - 1 do
            let ntypes = m.Namespaces.[ni].TypeDefinitions
            for i = 0 to ntypes.Length - 1 do
                let ti = Index(typei + uint32 i)
                types.[ti] <- RuntimeTypeDefinition(rm, ntypes.[i], Index methodi)
                let ms = ntypes.[i].Methods
                for mi = 0 to ms.Length - 1 do methods.[Index(methodi + uint32 mi)] <- ti
                methodi <- methodi + uint32 ms.Length
            typei <- typei + uint32 ntypes.Length

        types, methods

    member _.Identifier = m.Header.Module

    member _.IdentifierAt(Index i: IdentifierIndex) = m.Identifiers.Identifiers.[Checked.int32 i]
    member _.CodeAt(Index i: CodeIndex) = m.Code.[Checked.int32 i]
    member _.TypeSignatureAt(Index i: TypeSignatureIndex) = m.TypeSignatures.[Checked.int32 i]
    member _.MethodSignatureAt(Index i: MethodSignatureIndex) = m.MethodSignatures.[Checked.int32 i]

    member _.InitializeType i =
        match typeDefLookup.TryGetValue i with
        | true, existing -> existing
        | false, _ -> // Assume it is a type import.
            failwith "TODO: Type imports not yet implemented."

    member this.InitializeMethod i =
        match methodDefLookup.TryGetValue i with
        | true, rt -> this.InitializeType(rt).InitializeMethod i
        | false, _ -> // Assume it is a method import.
            failwith "TODO: Method imports not yet implemented."

    interface IEquatable<RuntimeModule> with member this.Equals other = this.Identifier = other.Identifier

let executionEntryPoint (program: Module) (moduleImportLoader: ModuleImport -> Module) =
    let moduleImportResolver =
        let resolved = Dictionary<ModuleImport, RuntimeModule> program.Imports.Length
        let rec resolver import =
            match resolved.TryGetValue import with
            | true, existing -> existing
            | false, _ ->
                let r = RuntimeModule(moduleImportLoader import, resolver)
                resolved.Add(import, r)
                r
        resolver

    match program.EntryPoint with
    | ValueSome main ->
        let application = RuntimeModule(program, moduleImportResolver)
        let main' = application.InitializeMethod main

        // TODO: Check signature of method to determine if argv should be passed.
        let arguments (args: ImmutableArray<_>) = if not args.IsDefaultOrEmpty then invalidOp "TODO: Passing of arguments for main not supported"

        let start = RuntimeStackFrame(ValueNone, ImmutableArray.Empty, ImmutableArray.Empty, ValueNone)
        let result = main'.Invoke(start, arguments)

        match result.Length with
        | 0 -> 1
        | 1 -> // TODO: Check signature of method to determine if a 32bit integer exit code is returned
            match result.[0] with
            | RuntimeRegister.R4 { contents = ecode } -> int32 ecode
        | _ -> failwith "TODO: Multiple exit codes on entry point not supported"
    | ValueNone -> failwithf "TODO: Error for missing entry point method"

let run program (importDirs: IReadOnlyCollection<DirectoryInfo>) =
    executionEntryPoint (ParseModule.fromFile program) <| fun _ ->
        failwith "TODO: Implement resolution of references"
