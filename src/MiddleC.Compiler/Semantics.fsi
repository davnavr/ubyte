namespace MiddleC.Compiler.Semantics

open System.Collections.Immutable
open System.Runtime.CompilerServices

open UByte.Format

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
    | DuplicateTypeDefinition of FullTypeIdentifier
    | UndefinedTypeIdentifier of TypeIdentifier
    | AmbiguousTypeIdentifier of TypeIdentifier * matches: seq<FullTypeIdentifier>
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
    member Flags : Model.TypeDefinitionFlags

    override Equals : obj -> bool
    override GetHashCode : unit -> int32

type NamedType = Choice<CheckedTypeDefinition, UByte.Resolver.ResolvedTypeDefinition>

[<Sealed>]
type CheckedMethod =
    member Name : IdentifierNode
    member Flags : Model.MethodFlags

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
