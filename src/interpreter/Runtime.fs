module rec UByte.Interpreter.Runtime

open System.Collections.Generic
open System.IO

open UByte.Format
open UByte.Format.Model

[<Sealed>]
type RuntimeMethod (rmodule: RuntimeModule, method: Method) = class end

[<Sealed>]
type RuntimeTypeDefinition (rmodule: RuntimeModule, t: TypeDefinition, mstart: MethodIndex) = class end

[<Sealed>]
type RuntimeModule (m: Module, moduleImportResolver: ModuleImport -> RuntimeModule) =
    let typeDefLookup = Dictionary<TypeDefinitionIndex, RuntimeTypeDefinition>()
    let methodDefLookup = Dictionary<MethodIndex, TypeDefinitionIndex>()

    let countTypeImports, findTypeDef =
        let countTypeImports =
            let mutable starti = ValueNone
            fun() ->
                match starti with
                | ValueNone ->
                    let mutable sum = 0
                    for mimport in m.Imports do
                        for ns in mimport.ImportedNamespaces do sum <- sum + ns.TypeImports.Length + ns.TypeAliases.Length
                    starti <- ValueSome sum
                    sum
                | ValueSome i -> i

        let findTypeDef =
            let lookup = Dictionary<TypeDefinitionIndex, struct(int32 * int32 * MethodIndex)>()
            let mutable start, methodi, namespacei, typei = Index 0u, 0u, 0, 0
            let current() = m.Namespaces.[namespacei].TypeDefinitions.[typei]
            fun i ->
                match lookup.TryGetValue i with
                | true, (ni, ti, mi) -> struct(m.Namespaces.[ni].TypeDefinitions.[ti], mi)
                | false, _ ->
                    if i <= start then
                        start <- countTypeImports() |> uint32 |> Index
                        methodi <- 0u //countTypeImportsAndGetMethodStart
                        namespacei <- 0
                        typei <- 0

                    while start <> i && namespacei < m.Namespaces.Length do
                        // TODO: Account for type aliasas
                        while start <> i && typei < m.Namespaces.[namespacei].TypeDefinitions.Length do
                            let (Index starti) = start
                            start <- Index(Checked.(+) starti 1u)
                            if start <> i then
                                for mi = 0 to current().Methods.Length - 1 do
                                    methodDefLookup.[Index methodi] <- Index starti
                                    methodi <- methodi + 1u
                                typei <- typei + 1

                        if start <> i then namespacei <- namespacei + 1

                    let found = current()
                    let methodi' = Index methodi
                    lookup.Add(i, struct(namespacei, typei, methodi'))
                    struct(found, methodi')

        countTypeImports, findTypeDef

    member this.InitializeType i =
        match typeDefLookup.TryGetValue i with
        | true, existing -> existing
        | false, _ ->
            let struct(t, mstart) = findTypeDef i
            let rt = RuntimeTypeDefinition(this, t, mstart)
            typeDefLookup.Add(i, rt)
            //rt.InvokeInitializer
            rt

    member this.InitializeMethod i =
        match methodDefLookup.TryGetValue i with
        | true, method -> method
        | false, _ ->
            let rt = 
            ()

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
        application.InitializeMethod main
        -1
    | ValueNone -> failwithf "TODO: Error for missing entry point method"

let run program importDirs =
    executionEntryPoint (ParseModule.fromFile program) <| fun _ ->
        failwith "TODO: Implement resolution of references"
