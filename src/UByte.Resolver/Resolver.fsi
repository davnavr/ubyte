namespace UByte.Resolver

open System
open System.Collections.Immutable
open System.Collections.Generic

open UByte.Format.Model

[<Interface>]
type IResolutionLogger =
    member Log : string -> unit

[<Sealed>]
type ResolvedModule =
    new: Module * importer: (ModuleIdentifier -> ResolvedModule) -> ResolvedModule

    member Name : string
    member Version : VersionNumbers

    override ToString: unit -> string

[<Sealed>]
type ResolvedMethod =
    member Name : string
    member IsInstance : bool
    member IsConstructor : bool
    member IsVirtual : bool

    override ToString: unit -> string

[<Sealed>]
type ResolvedField =
    member Name : string
    member IsStatic : bool
    member IsMutable : bool

[<Sealed>]
type ResolvedTypeDefinition =
    //member Namespace : ImmutableArray<string>
    member Name : string
    member InheritedTypes : ImmutableArray<ResolvedTypeDefinition>
    member VTable: IReadOnlyDictionary<ResolvedMethod, ResolvedMethod>

    member FindMethod : name: string -> ResolvedMethod

    override ToString: unit -> string

type ResolvedField with
    member DeclaringType : ResolvedTypeDefinition
    member DeclaringModule : ResolvedModule

type ResolvedMethod with
    member DeclaringType : ResolvedTypeDefinition
    member DeclaringModule : ResolvedModule

[<Sealed; Class>]
type ModuleNotFoundException =
    inherit Exception

    member Name : ModuleIdentifier

type ResolvedModule with
    // TODO: Have events for when type is loaded.

    member TypeAt : index: TypeDefinitionIndex -> ResolvedTypeDefinition
    member MethodAt : index: MethodIndex -> ResolvedMethod
    member FieldAt : index: FieldIndex -> ResolvedField

    member FindType : typeNamespace: string * typeName: string -> ResolvedTypeDefinition
