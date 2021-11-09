[<RequireQualifiedAccess>]
module UByte.Runtime.Interpreter

open UByte.Format

[<Sealed; Class>]
type MissingEntryPointException = inherit System.Exception

[<Sealed; Class>]
type ModuleNotFoundException =
    inherit System.Exception

    member Name : Model.ModuleIdentifier

[<Literal>] val DefaultStackCapacity : int32 = 0xFFFFF

[<Sealed>]
type Runtime =
    /// Initializes the interpreter with the specified program containing the code to run.
    static member Initialize :
        program: Model.Module *
        ?moduleImportLoader: (Model.ModuleIdentifier -> Model.Module voption) *
        ?garbageCollectorStrategy: UByte.Runtime.MemoryManagement.IGarbageCollector -> Runtime

    member Program : UByte.Resolver.ResolvedModule

    /// <summary>Invokes the entry point of the program, supplying the specified arguments.</summary>
    /// <returns>The integer exit code returned by the program.</returns>
    /// <exception cref="T:UByte.Runtime.Interpreter.MissingEntryPointException" />
    member InvokeEntryPoint : argv: string[] * ?maxStackCapacity: int32 -> int32

    interface System.IDisposable
