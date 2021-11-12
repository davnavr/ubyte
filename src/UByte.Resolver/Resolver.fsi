namespace UByte.Resolver

open System
open System.Collections.Immutable
open System.Collections.Generic

open UByte.Format.Model

[<Sealed>]
type ResolvedModule =
    new : Module * importer: (ModuleIdentifier -> ResolvedModule) -> ResolvedModule

    member Identifier : ModuleIdentifier
    member Name : string

    override ToString: unit -> string

[<Sealed>]
type ResolvedMethod =
    member Name : string
    member Body : MethodBody
    member IsInstance : bool
    member IsConstructor : bool
    member IsVirtual : bool
    member IsExternal : bool
    member Visibility : VisibilityFlags
    member Signature : MethodSignature

    override ToString: unit -> string

[<Sealed>]
type ResolvedField =
    member Name : string
    member FieldType : AnyType
    member IsStatic : bool
    member IsMutable : bool
    member Visibility : VisibilityFlags

[<Sealed>]
type ResolvedTypeDefinition =
    //member Namespace : ImmutableArray<string>
    member Name : string
    member Index : TypeDefinitionIndex
    member DeclaringModule : ResolvedModule
    /// The types that the current type directly inherits from.
    member BaseTypes : ImmutableArray<ResolvedTypeDefinition>
    member VTable: IReadOnlyDictionary<ResolvedMethod, ResolvedMethod>
    member DefinedFields : ImmutableArray<ResolvedField>

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

    new : identifier: ModuleIdentifier * message: string -> ModuleNotFoundException

    member Identifier : ModuleIdentifier

[<Sealed; Class>]
type TypeNotFoundException =
    inherit Exception

    new : ResolvedModule * typeNamespace: string * typeName: string * message: string -> TypeNotFoundException

    member Module : ResolvedModule
    member TypeNamespace : string
    member TypeName : string

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ModuleResolutionEventArguments<'Import> =
    { Originator: ResolvedModule
      Import: 'Import }

[<NoComparison; NoEquality>]
type ResolutionEventArguments<'Owner, 'Import> =
    { Originator: ResolvedModule
      Owner: 'Owner
      Import: 'Import }

type IResolutionEvent<'Owner, 'T> = IEvent<ResolutionEventArguments<'Owner, 'T>>

type ResolvedModule with
    [<CLIEvent>] member ModuleResolving : IEvent<ModuleResolutionEventArguments<ModuleIdentifier>>
    [<CLIEvent>] member ModuleResolved : IEvent<ModuleResolutionEventArguments<ResolvedModule>>
    [<CLIEvent>] member TypeResolving : IResolutionEvent<ResolvedModule, Choice<TypeDefinition, TypeDefinitionImport>>
    [<CLIEvent>] member TypeResolved : IEvent<ResolvedTypeDefinition>
    [<CLIEvent>] member MethodResolving : IResolutionEvent<ResolvedTypeDefinition, Choice<Method, MethodImport>>
    [<CLIEvent>] member MethodResolved : IEvent<ResolvedMethod>
    [<CLIEvent>] member FieldResolving : IResolutionEvent<ResolvedTypeDefinition, Choice<Field, FieldImport>>
    [<CLIEvent>] member FieldResolved : IEvent<ResolvedField>

    member EntryPoint : ResolvedMethod voption

    member TypeAt : index: TypeDefinitionIndex -> ResolvedTypeDefinition
    member MethodAt : index: MethodIndex -> ResolvedMethod
    member FieldAt : index: FieldIndex -> ResolvedField

    member TypeSignatureAt : index: TypeSignatureIndex -> AnyType
    member MethodSignatureAt : index: MethodSignatureIndex -> MethodSignature
    member IdentifierAt : index: IdentifierIndex -> string
    member DataAt : index: DataIndex -> ImmutableArray<byte>
    member CodeAt : index: CodeIndex -> Code

    /// Finds the first type defined in this module with the specified name.
    member FindType : typeNamespace: string * typeName: string -> ResolvedTypeDefinition
