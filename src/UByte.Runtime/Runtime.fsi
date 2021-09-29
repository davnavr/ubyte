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
    member ArgumentRegisters : ImmutableArray<RuntimeRegister>
    member LocalRegisters : ImmutableArray<RuntimeRegister>
    /// Contains the registers that the return values will be stored to.
    member ReturnRegisters : ImmutableArray<RuntimeRegister>
    member Previous : RuntimeStackFrame voption
    member InstructionIndex : int32

    member RegisterAt : Model.RegisterIndex -> RuntimeRegister

[<Class>]
type RuntimeException =
    inherit Exception

    member CurrentFrame : RuntimeStackFrame voption

[<Sealed; Class>]
type InvalidConstructorException = inherit RuntimeException

[<Sealed>]
type RuntimeMethod =
    member Name : string

    member IsInstance : bool

    /// Indicates whether the specified method is a constructor.
    member IsConstructor : bool

type InvalidConstructorException with
    /// The method that is not a valid constructor.
    member Method : RuntimeMethod

[<Sealed>]
type RuntimeField =
    member Name : string

[<RequireQualifiedAccess>]
module Interpreter =
    /// <summary>Interprets a method.</summary>
    /// <param name="returns">
    /// An array of registers that will contain the return values after the <paramref name="entrypoint"/> method is interpreted.
    /// </param>
    /// <param name="arguments">
    /// An array of registers containing the arguments to pass to the <paramref name="entrypoint"/> method.
    /// </param>
    /// <param name="entrypoint">The method to call and interpret.</param>
    val interpret :
        returns: ImmutableArray<RuntimeRegister> ->
        arguments: ImmutableArray<RuntimeRegister> ->
        entrypoint: RuntimeMethod ->
        unit

type RuntimeStackFrame with member CurrentMethod : RuntimeMethod

[<Sealed>]
type RuntimeTypeDefinition =
    //member Namespace : ImmutableArray<string>

    member Name : string

    //member InvokeInitializer: unit -> 

type RuntimeMethod with member DeclaringType : RuntimeTypeDefinition

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

type MissingEntryPointException with
    /// The module that is missing an entry point.
    member Module : RuntimeModule

type RuntimeMethod with member Module : RuntimeModule
type RuntimeTypeDefinition with member Module : RuntimeModule

val initialize : program: Model.Module -> moduleImportLoader: (Model.ModuleIdentifier -> Model.Module) -> RuntimeModule
