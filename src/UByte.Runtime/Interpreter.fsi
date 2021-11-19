[<RequireQualifiedAccess>]
module UByte.Runtime.Interpreter

open UByte.Format
open UByte.Resolver
open UByte.Runtime.MemoryManagement

[<Sealed; Class>]
type MissingEntryPointException = inherit System.Exception

[<Sealed; Class>]
type ModuleNotFoundException =
    inherit System.Exception

    member Name : Model.ModuleIdentifier

[<Literal>] val DefaultStackCapacity : int32 = 0xFFFFF

[<Sealed>]
type StackFrame =
    member CurrentMethod : ResolvedMethod
    member CurrentModule : ResolvedModule
    member Previous : StackFrame voption
    member PreviousMethod : ResolvedMethod voption
    member StackTrace : string

[<Sealed>]
type EventSource =
    [<CLIEvent>] member MethodCalled : IEvent<StackFrame>
    [<CLIEvent>] member MethodReturned : IEvent<StackFrame>

[<Sealed; Class>]
type UncaughtProgramException =
    inherit System.Exception

    /// The stack frame describing where the exception originated from.
    member ProgramFrame : StackFrame

[<Sealed>]
type Runtime =
    /// Initializes the interpreter with the specified program containing the code to run.
    static member Initialize :
        program: Model.Module *
        ?moduleImportLoader: (Model.ModuleIdentifier -> Model.Module voption) *
        ?garbageCollectorStrategy: IGarbageCollector -> Runtime

    member Program : ResolvedModule

    /// <summary>Invokes the entry point of the program, supplying the specified arguments.</summary>
    /// <param name="argv">The arguments to pass to the entry-point method of the program.</param>
    /// <param name="maxStackCapacity">Specifies the size, in bytes, of the stack.</param>
    /// <param name="interpreterEventHandler" />
    /// <param name="stackEventHandler" />
    /// <returns>The integer exit code returned by the program.</returns>
    /// <exception cref="T:UByte.Runtime.Interpreter.MissingEntryPointException" />
    /// <exception cref="T:UByte.Runtime.Interpreter.UncaughtProgramException">
    /// Thrown when an exception was thrown by the interpreted program that was not caught.
    /// </exception>
    member InvokeEntryPoint :
        argv: string[] *
        ?maxStackCapacity: int32 *
        ?interpreterEventHandler: (EventSource -> unit) *
        ?stackEventHandler: (ValueStack -> unit) -> int32

    interface System.IDisposable
