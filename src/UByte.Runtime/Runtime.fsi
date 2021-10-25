[<RequireQualifiedAccess>]
module UByte.Interpreter.Runtime

open System
open System.Collections.Immutable

open UByte.Format

[<NoComparison; NoEquality>]
type RuntimeRegister

type RuntimeRegister with
    override ToString : unit -> string

    interface IEquatable<RuntimeRegister>

[<Sealed>]
type RuntimeStackFrame =
    member ArgumentRegisters : ImmutableArray<RuntimeRegister>
    /// Contains the registers that the return values will be stored to.
    member ReturnRegisters : ImmutableArray<RuntimeRegister>
    member Previous : RuntimeStackFrame voption
    member BlockIndex : int32 with get
    member InstructionIndex : int32 with get
    member StackTrace : string

[<Class>]
type RuntimeException =
    inherit Exception

    member CurrentFrame : RuntimeStackFrame voption

[<Sealed>]
type RuntimeMethod =
    member Name : string

    member IsInstance : bool

    /// Indicates whether the specified method is a constructor.
    member IsConstructor : bool

    member IsVirtual : bool

    override ToString: unit -> string

[<Sealed; Class>]
type InvalidConstructorException =
    inherit RuntimeException

    /// The method that is not a valid constructor.
    member Method : RuntimeMethod

[<Sealed; Class>]
type AbstractMethodCallException =
    inherit RuntimeException

    member Method : RuntimeMethod

[<Sealed>]
type RuntimeField =
    member Name : string
    member IsStatic : bool
    member IsMutable : bool

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

    member InheritedTypes : ImmutableArray<RuntimeTypeDefinition>

    member FindMethod : name: string -> RuntimeMethod

    //member InvokeInitializer: unit -> 

    member VTable: System.Collections.Generic.IReadOnlyDictionary<RuntimeMethod, RuntimeMethod>

    override ToString: unit -> string

[<Sealed; Class>]
type RecursiveInheritanceException =
    inherit RuntimeException

    member Type: RuntimeTypeDefinition

type RuntimeMethod with member DeclaringType : RuntimeTypeDefinition
type RuntimeField with member DeclaringType : RuntimeTypeDefinition

[<Sealed; Class>]
type TypeNotFoundException =
    inherit RuntimeException

    member TypeNamespace: string
    member TypeName: string

[<Sealed>]
type RuntimeModule =
    member Name : string

    member Version : Model.VersionNumbers

    member InitializeType : Model.TypeDefinitionIndex -> RuntimeTypeDefinition

    member InitializeMethod : Model.MethodIndex -> RuntimeMethod

    member InitializeField : Model.FieldIndex -> RuntimeField

    member FindType : typeNamespace: string * typeName: string -> RuntimeTypeDefinition

    /// <summary>Invokes the entry point of the program, supplying the specified arguments.</summary>
    /// <exception cref="T:UByte.Interpreter.Runtime.MissingEntryPointException" />
    member InvokeEntryPoint : argv: string[] -> int32

    override ToString: unit -> string

[<Sealed; Class>]
type MissingEntryPointException =
    inherit RuntimeException

    /// The module that is missing an entry point.
    member Module : RuntimeModule

type TypeNotFoundException with
    /// The module that was searched for the specified type.
    member Module : RuntimeModule

type RuntimeMethod with member Module : RuntimeModule
type RuntimeTypeDefinition with member Module : RuntimeModule

[<Sealed; Class>]
type ModuleNotFoundException =
    inherit RuntimeException

    member Name: Model.ModuleIdentifier

val initialize : program: Model.Module -> moduleImportLoader: (Model.ModuleIdentifier -> Model.Module voption) -> RuntimeModule
