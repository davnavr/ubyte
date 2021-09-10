module rec UByte.Interpreter.Runtime

open System.Collections.Generic
open System.Collections.Immutable
open System.IO

open UByte.Format
open UByte.Format.Model

[<Sealed>]
type RuntimeRegister = class end

[<Sealed>]
type RuntimeStackFrame (prev: RuntimeStackFrame voption, args: ImmutableArray<RuntimeRegister>, method: RuntimeMethod voption) =
    member _.MethodArguments = args
    member val Registers = failwith "bad": ImmutableArray<_>
    member _.CurrentMethod = method
    member _.Previous = prev

    member this.RegisterAt (Index i: RegisterIndex) =
        let i' = Checked.int32 i
        if i' >= args.Length then this.Registers.[i' - args.Length] else args.[i']

type MethodInvocationResult = ImmutableArray<RuntimeRegister>

[<Sealed>]
type RuntimeMethod (rmodule: RuntimeModule, method: Method) =
    member _.Module = rmodule

    member _.Invoke(current: RuntimeStackFrame) =
        failwith "bad"

[<Sealed>]
type RuntimeTypeDefinition (rmodule: RuntimeModule, t: TypeDefinition, mstart: MethodIndex) =
    let (Index mstart) = mstart
    let methods = Dictionary<MethodIndex, RuntimeMethod> t.Methods.Length

    member val Name = rmodule.IdentifierAt t.TypeName

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

    member _.IdentifierAt(Index i: IdentifierIndex) = m.Identifiers.Identifiers.[Checked.int32 i]

    member _.CodeAt(Index i: CodeIndex) = m.Code.[Checked.int32 i]

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

        -1
    | ValueNone -> failwithf "TODO: Error for missing entry point method"

let run program importDirs =
    executionEntryPoint (ParseModule.fromFile program) <| fun _ ->
        failwith "TODO: Implement resolution of references"
