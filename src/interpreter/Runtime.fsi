[<RequireQualifiedAccess>]
module UByte.Interpreter.Runtime

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO

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

type MethodInvocationResult = ImmutableArray<RuntimeRegister>

[<RequireQualifiedAccess>]
module Interpreter =
    val interpret :
        current: RuntimeStackFrame ->
        instructions: ImmutableArray<Model.InstructionSet.Instruction> ->
        MethodInvocationResult

[<Sealed>]
type RuntimeMethod =
    member Name : Model.Name

    member Invoke : previous: RuntimeStackFrame * arguments: (ImmutableArray<RuntimeRegister> -> unit) -> MethodInvocationResult

type RuntimeStackFrame with member CurrentMethod : RuntimeMethod voption

[<Sealed>]
type RuntimeTypeDefinition =
    //member Namespace : ImmutableArray<string>

    member Name : Model.Name

    //member InvokeInitializer: unit -> 

    member InitializeMethod : Model.MethodIndex -> RuntimeMethod

[<Sealed>]
type RuntimeModule =
    member InitializeType : Model.TypeDefinitionIndex -> RuntimeTypeDefinition

    member InitializeMethod : Model.MethodIndex -> RuntimeMethod

    interface IEquatable<RuntimeModule>

type RuntimeMethod with member Module : RuntimeModule
type RuntimeTypeDefinition with member Module : RuntimeModule

val run : program: FileInfo -> importDirs: IReadOnlyCollection<DirectoryInfo> -> int32
