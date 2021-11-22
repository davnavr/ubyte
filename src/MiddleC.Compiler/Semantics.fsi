namespace MiddleC.Compiler.Semantics

open System.Collections.Immutable
open System.Diagnostics
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

[<IsReadOnly; Struct; NoComparison; StructuralEquality; DebuggerDisplay("{ToString()}")>]
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
    member Visibility : UByte.Format.Model.VisibilityFlags

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

[<RequireQualifiedAccess; NoComparison; NoEquality; DebuggerDisplay("{ToString()}")>]
type CheckedExpression =
    | LiteralBoolean of bool
    | LiteralSignedInteger of int64
    //| LiteralUnsignedInteger of uint64
    
    override ToString : unit -> string

[<RequireQualifiedAccess; NoComparison; NoEquality; DebuggerDisplay("{ToString()}")>]
type TypedExpression =
    { Expression: CheckedExpression
      Type: CheckedType
      Node: ParsedNode<ExpressionNode> }

    override ToString : unit -> string

/// Simplified representation of a statement that has been type checked, constructs such as if-statements and while loops are
/// lowered.
[<RequireQualifiedAccess; NoComparison; NoEquality; DebuggerDisplay("{ToString()}")>]
type CheckedStatement =
    | Return of ImmutableArray<TypedExpression>
    | Empty

    override ToString : unit -> string

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type CheckedMethodBody =
    | Defined of ImmutableArray<CheckedStatement>
    | External of string * library: string

[<RequireQualifiedAccess; IsReadOnly; Struct; NoComparison; StructuralEquality>]
type CheckedMethodSignature =
    { ParameterTypes: ImmutableArray<CheckedType>
      ReturnTypes: ImmutableArray<CheckedType> }

    interface System.IEquatable<CheckedMethodSignature>

[<Sealed>]
type CheckedMethod =
    member DeclaringType : CheckedTypeDefinition
    member Name : IdentifierNode
    member Flags : UByte.Format.Model.MethodFlags
    member Visibility : UByte.Format.Model.VisibilityFlags
    member Parameters : ImmutableArray<CheckedParameter>
    member ReturnTypes : ImmutableArray<CheckedType>
    member Signature : CheckedMethodSignature
    member Body : CheckedMethodBody

type NamedMethod = Choice<CheckedMethod, UByte.Resolver.ResolvedMethod>

type CheckedTypeDefinition with
    member InheritedTypes : ImmutableArray<NamedType>
    member Methods : ImmutableArray<CheckedMethod>

[<Sealed>]
type CheckedModule =
    member Name : UByte.Format.Model.Name
    member Version : UByte.Format.Model.VersionNumbers
    member Identifier : UByte.Format.Model.ModuleIdentifier
    member DefinedTypes : ImmutableArray<CheckedTypeDefinition>
    member EntryPoint : CheckedMethod voption
    member Errors : ImmutableArray<SemanticError>
    member ImportedModules : ImmutableArray<UByte.Resolver.ResolvedModule>

[<RequireQualifiedAccess>]
module CheckedType =
    val primitive : UByte.Format.Model.PrimitiveType -> CheckedType

[<RequireQualifiedAccess>]
module TypeChecker =
    val check :
        name: string ->
        version: seq<uint32> ->
        files: ImmutableArray<ParsedFile> ->
        imports: ImmutableArray<UByte.Resolver.ResolvedModule> ->
        CheckedModule
