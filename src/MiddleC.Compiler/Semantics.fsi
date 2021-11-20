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

[<NoComparison; NoEquality>]
type CheckedTypeDefinition

type CheckedTypeDefinition with
    member Identifier : FullTypeIdentifier

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type CheckedModule =
    { DefinedTypes: ImmutableArray<CheckedTypeDefinition>
      Errors: ImmutableArray<SemanticError> }

[<RequireQualifiedAccess>]
module TypeChecker =
    val check : files: ImmutableArray<ParsedFile> -> imports: ImmutableArray<UByte.Resolver.ResolvedModule> -> CheckedModule
