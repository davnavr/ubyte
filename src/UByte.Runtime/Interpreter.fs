[<RequireQualifiedAccess>]
module UByte.Runtime.Interpreter

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open UByte.Format.Model

open UByte.Resolver
open UByte.Runtime.MemoryManagement

let inline isFlagSet flag value = value &&& flag = flag

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type RuntimeRegister = { Value: unativeint; Type: RegisterType }

let [<Literal>] private MaxStackCapacity = 0xFFFFF

let private interpret
    (gc: IGarbageCollector)
    (arguments: ImmutableArray<RuntimeRegister>)
    (entrypoint: ResolvedMethod)
    =
    use stack = new ValueStack(MaxStackCapacity)
    failwith "bad"

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
