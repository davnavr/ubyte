[<RequireQualifiedAccess>]
module UByte.Interpreter.Runtime

open System
open System.Collections.Immutable

open UByte.Format

[<NoComparison; NoEquality>]
type RuntimeRegister

type RuntimeRegister with
    member CopyValueTo : destination: RuntimeRegister -> unit

[<Sealed>]
type RuntimeStackFrame =
    member MethodArguments : ImmutableArray<RuntimeRegister>
    member Registers : ImmutableArray<RuntimeRegister>
    member Previous : RuntimeStackFrame voption

    member RegisterAt : Model.RegisterIndex -> RuntimeRegister

[<Class>]
type RuntimeException =
    inherit Exception

    member CurrentFrame : RuntimeStackFrame voption

type MethodInvocationResult = ImmutableArray<RuntimeRegister>

[<Sealed>]
type RuntimeMethod =
    member Name : string

    member Invoke : previous: RuntimeStackFrame * arguments: (ImmutableArray<RuntimeRegister> -> unit) -> MethodInvocationResult

[<Sealed>]
type RuntimeField =
    member Name : string

[<RequireQualifiedAccess>]
module Interpreter =
    val interpret :
        current: RuntimeStackFrame ->
        instructions: ImmutableArray<Model.InstructionSet.Instruction> ->
        methodIndexResolver: (Model.MethodIndex -> RuntimeMethod) ->
        MethodInvocationResult

type RuntimeStackFrame with member CurrentMethod : RuntimeMethod voption

[<Sealed>]
type RuntimeTypeDefinition =
    //member Namespace : ImmutableArray<string>

    member Name : string

    //member InvokeInitializer: unit -> 

/// Thrown when the entry point of a module could not be found.
[<Sealed; Class>]
type MissingEntryPointException = inherit RuntimeException

[<Sealed>]
type RuntimeModule =
    member InitializeType : Model.TypeDefinitionIndex -> RuntimeTypeDefinition

    member InitializeMethod : Model.MethodIndex -> RuntimeMethod

    member InitializeField : Model.FieldIndex -> RuntimeField

    /// <summary>Invokes the entry point of the program, supplying the specified arguments.</summary>
    /// <exception cref="T:UByte.Interpreter.Runtime.MissingEntryPointException" />
    member InvokeEntryPoint : argv: string[] -> int32

type RuntimeMethod with member Module : RuntimeModule
type RuntimeTypeDefinition with member Module : RuntimeModule

val initialize : program: Model.Module -> moduleImportLoader: (Model.ModuleIdentifier -> Model.Module) -> RuntimeModule
