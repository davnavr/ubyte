namespace MiddleC.Compiler.Semantics

open System.Collections.Immutable
open System.Runtime.CompilerServices

open MiddleC.Compiler.Parser

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type FullNamespaceName =
    internal
    | FullNamespaceName of ParsedNamespaceName

    interface System.IEquatable<FullNamespaceName>

[<RequireQualifiedAccess>]
module FullNamespaceName =
    val empty : FullNamespaceName
    val append : nested: ParsedNamespaceName -> parent: FullNamespaceName -> FullNamespaceName
    val toParsedName : FullNamespaceName -> ParsedNamespaceName

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type FullTypeIdentifier =
    internal
    | FullTypeIdentifier of TypeIdentifier

    member Name : IdentifierNode

    member Namespace : FullNamespaceName

    override ToString : unit -> string

    interface System.IEquatable<FullTypeIdentifier>

[<NoComparison; NoEquality>]
type SemanticErrorMessage =
    | AmbiguousTypeIdentifier of TypeIdentifier * matches: seq<FullTypeIdentifier>
    | DuplicateParameter of ParsedIdentifier
    | DuplicateTypeDefinition of FullTypeIdentifier
    | MultipleEntryPoints
    | UndefinedTypeIdentifier of TypeIdentifier
    | UnknownError of message: string

    override ToString : unit -> string

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type SemanticError =
    { Line: uint32
      Column: uint32
      Source: ParsedFile
      Message: SemanticErrorMessage }

[<Sealed>]
type CheckedTypeDefinition =
    interface System.IEquatable<CheckedTypeDefinition>

    member Identifier : FullTypeIdentifier
    member Flags : UByte.Format.Model.TypeDefinitionFlags

    override Equals : obj -> bool
    override GetHashCode : unit -> int32

type NamedType = Choice<CheckedTypeDefinition, UByte.Resolver.ResolvedTypeDefinition>

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type CheckedValueType =
    | Primitive of UByte.Format.Model.PrimitiveType

    interface System.IEquatable<CheckedValueType>

/// Represents a type, modeled after the type system of the binary format.
[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type CheckedType =
    | ValueType of CheckedValueType

    interface System.IEquatable<CheckedType>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type CheckedParameter =
    { Name: IdentifierNode
      Type: CheckedType }

[<Sealed>]
type CheckedMethod =
    member DeclaringType : CheckedTypeDefinition
    member Name : IdentifierNode
    member Flags : UByte.Format.Model.MethodFlags
    member Parameters : ImmutableArray<CheckedParameter>
    member ReturnTypes : ImmutableArray<CheckedType>

type NamedMethod = Choice<CheckedMethod, UByte.Resolver.ResolvedMethod>

type CheckedTypeDefinition with
    member InheritedTypes : ImmutableArray<NamedType>
    member Methods : ImmutableArray<CheckedMethod>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type CheckedModule = // TODO: Make this a normal Sealed class to prevent construction elsewhere.
    { DefinedTypes: ImmutableArray<CheckedTypeDefinition>
      Errors: ImmutableArray<SemanticError> }

[<RequireQualifiedAccess>]
module TypeChecker =
    val check : files: ImmutableArray<ParsedFile> -> imports: ImmutableArray<UByte.Resolver.ResolvedModule> -> CheckedModule
