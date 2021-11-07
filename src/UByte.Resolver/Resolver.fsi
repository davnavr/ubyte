namespace UByte.Resolver

open System
open System.Collections.Immutable
open System.Collections.Generic

open UByte.Format.Model

[<Sealed>]
type ResolvedModule =
    new: Module * importer: (ModuleIdentifier -> ResolvedModule) -> ResolvedModule

    member Identifier : ModuleIdentifier
    member Name : string

    override ToString: unit -> string

[<Sealed>]
type ResolvedMethod =
    member Name : string
    member IsInstance : bool
    member IsConstructor : bool
    member IsVirtual : bool
    member IsExternal : bool
    member Visibility : VisibilityFlags

    override ToString: unit -> string

[<Sealed>]
type ResolvedField =
    member Name : string
    member IsStatic : bool
    member IsMutable : bool
    member Visibility : VisibilityFlags

[<Sealed>]
type ResolvedTypeDefinition =
    //member Namespace : ImmutableArray<string>
    member Name : string
    member DeclaringModule : ResolvedModule
    /// The types that the current type directly inherits from.
    member BaseTypes : ImmutableArray<ResolvedTypeDefinition>
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

    new: identifier: ModuleIdentifier * message: string -> ModuleNotFoundException

    member Identifier : ModuleIdentifier

[<Sealed; Class>]
type TypeNotFoundException =
    inherit Exception

    new: ResolvedModule * typeNamespace: string * typeName: string * message: string -> TypeNotFoundException

    member Module : ResolvedModule
    member TypeNamespace : string
    member TypeName : string

type ResolvedModule with
    [<CLIEvent>] member ModuleResolved : IEvent<ResolvedModule>
    [<CLIEvent>] member TypeResolved : IEvent<ResolvedTypeDefinition>
    [<CLIEvent>] member MethodResolved : IEvent<ResolvedMethod>
    [<CLIEvent>] member FieldResolved : IEvent<ResolvedField>

    member TypeAt : index: TypeDefinitionIndex -> ResolvedTypeDefinition
    member MethodAt : index: MethodIndex -> ResolvedMethod
    member FieldAt : index: FieldIndex -> ResolvedField

    member MethodSignatureAt : index: MethodSignatureIndex -> MethodSignature

    /// Finds the first type defined in this module with the specified name.
    member FindType : typeNamespace: string * typeName: string -> ResolvedTypeDefinition
