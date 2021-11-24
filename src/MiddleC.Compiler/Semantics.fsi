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

[<Sealed>]
type CheckedTypeDefinition =
    interface System.IEquatable<CheckedTypeDefinition>

    member Identifier : FullTypeIdentifier
    member Flags : UByte.Format.Model.TypeDefinitionFlags
    member Visibility : UByte.Format.Model.VisibilityFlags

    override Equals : obj -> bool
    override GetHashCode : unit -> int32

type NamedType = Choice<CheckedTypeDefinition, UByte.Resolver.ResolvedTypeDefinition>

[<RequireQualifiedAccess; NoComparison; StructuralEquality; DebuggerDisplay("{ToString()}")>]
type CheckedValueType =
    | Primitive of UByte.Format.Model.PrimitiveType

    override ToString : unit -> string

    interface System.IEquatable<CheckedValueType>

[<RequireQualifiedAccess; NoComparison; StructuralEquality; DebuggerDisplay("{ToString()}")>]
type CheckedElementType =
    | ValueType of CheckedValueType
    | ReferenceType of CheckedReferenceType

    override ToString : unit -> string

    interface System.IEquatable<CheckedElementType>

and [<RequireQualifiedAccess; NoComparison; StructuralEquality; DebuggerDisplay("{ToString()}")>] CheckedReferenceType =
    | Any
    | Array of CheckedElementType

    override ToString : unit -> string

    interface System.IEquatable<CheckedReferenceType>

/// Represents a type, modeled after the type system of the binary format.
[<RequireQualifiedAccess; NoComparison; StructuralEquality; DebuggerDisplay("{ToString()}")>]
type CheckedType =
    | ValueType of CheckedValueType
    | ReferenceType of CheckedReferenceType
    /// Used when calling a method that does not return any values.
    | Void

    override ToString : unit -> string

    interface System.IEquatable<CheckedType>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type CheckedParameter =
    { Name: IdentifierNode
      Type: CheckedType }

[<RequireQualifiedAccess; NoComparison; NoEquality; DebuggerDisplay("{ToString()}")>]
type CheckedExpression =
    | LiteralBoolean of bool
    | LiteralSignedInteger of int64
    | LiteralUnsignedInteger of uint64
    | Local of ParsedIdentifier // CheckedLocal
    | MethodCall of NamedMethod * arguments: ImmutableArray<TypedExpression>
    | NewArray of CheckedElementType * elements: ImmutableArray<TypedExpression>
    ///// Represents the absence of a value, used when a method that is called does not have any return values.
    //| Nothing

    override ToString : unit -> string

and [<RequireQualifiedAccess; NoComparison; NoEquality; DebuggerDisplay("{ToString()}")>] TypedExpression =
    { Expression: CheckedExpression
      Type: CheckedType
      Node: ParsedNode<ExpressionNode> }

    override ToString : unit -> string

and [<Sealed>] CheckedMethod =
    member DeclaringType : CheckedTypeDefinition
    member Name : IdentifierNode
    member Flags : UByte.Format.Model.MethodFlags
    member Visibility : UByte.Format.Model.VisibilityFlags
    member Parameters : ImmutableArray<CheckedParameter>
    member ReturnTypes : ImmutableArray<CheckedType>

and NamedMethod = Choice<CheckedMethod, UByte.Resolver.ResolvedMethod>

/// Simplified representation of a statement that has been type checked, constructs such as if-statements and while loops are
/// lowered.
[<RequireQualifiedAccess; NoComparison; NoEquality; DebuggerDisplay("{ToString()}")>]
type CheckedStatement =
    | Expression of TypedExpression
    | LocalDeclaration of constant: bool * name: IdentifierNode * CheckedType * value: TypedExpression
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

type CheckedMethod with
    member Signature : CheckedMethodSignature
    member Body : CheckedMethodBody

type CheckedTypeDefinition with
    member InheritedTypes : ImmutableArray<NamedType>
    member Methods : ImmutableArray<CheckedMethod>

[<NoComparison; NoEquality>]
type SemanticErrorMessage =
    | AmbiguousTypeIdentifier of TypeIdentifier * matches: seq<FullTypeIdentifier>
    | ArrayConstructorCall
    | DuplicateLocalDeclaration of ParsedIdentifier
    | DuplicateParameter of ParsedIdentifier
    | DuplicateTypeDefinition of FullTypeIdentifier
    | FirstClassFunctionsNotSupported
    | InvalidCharacterType of ParsedNode<AnyTypeNode>
    | InvalidElementType of CheckedType
    | MultipleEntryPoints
    | UndefinedTypeIdentifier of TypeIdentifier
    | UndefinedLocal of ParsedIdentifier
    | UndefinedMethod of FullTypeIdentifier * methodName: IdentifierNode
    | UnknownError of message: string

    override ToString : unit -> string

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type SemanticError =
    { Line: uint32
      Column: uint32
      Source: ParsedFile
      Message: SemanticErrorMessage }

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
