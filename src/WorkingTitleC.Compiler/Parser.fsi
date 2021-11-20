namespace WorkingTitleC.Compiler.Parser

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FParsec

[<RequireQualifiedAccess; IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type ParsedIdentifier =
    internal
    | Identifier of string

    override ToString : unit -> string

    interface IEquatable<ParsedIdentifier>

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type ParsedNode<'Content> =
    { Line: uint32
      Column: uint32
      Content: 'Content }

type IdentifierNode = ParsedNode<ParsedIdentifier>

type ParsedNodeArray<'Content> = ImmutableArray<ParsedNode<'Content>>

type ParsedNamespaceName = ParsedNodeArray<ParsedIdentifier>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type AnyTypeNode =
    | Primitive of UByte.Format.Model.PrimitiveType
    | Array of AnyTypeNode
    //| ObjectReference of Choice<TypeNode, IdentifierNode>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type StatementNode =
    | Empty

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type TypeAttributeNode =
    /// <summary>
    /// Indicates that instances of the type cannot be created, only instances of subtypes, implies the <c>inheritable</c>
    /// attribute.
    /// </summary>
    | Abstract
    | Inheritable

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type MethodAttributeNode =
    /// Indicates that the method is the entry point of the module, can only be applied to one method in an entire module.
    | Entrypoint
    | Instance
    | Abstract
    | Virtual

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type MethodBodyNode =
    | Defined of ParsedNodeArray<StatementNode>
    | External of ParsedNode<string> * library: ParsedNode<string>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type TypeMemberNode =
    //| FieldDeclaration of name: IdentifierNode *
    | MethodDeclaration of name: IdentifierNode * ParsedNodeArray<MethodAttributeNode> *
        parameters: ImmutableArray<IdentifierNode * ParsedNode<AnyTypeNode>> * ParsedNodeArray<AnyTypeNode> * MethodBodyNode
    //| ConstructorDeclaration of parameters: ParsedNodeArray<ParameterNode> * ParsedNodeArray<TypeNode>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type TopLevelNode =
    | UsingNamespace of ParsedNamespaceName
    | NamespaceDeclaration of ParsedNamespaceName * ParsedNodeArray<TopLevelNode>
    | TypeDeclaration of name: IdentifierNode * ParsedNodeArray<TypeAttributeNode> * ParsedNodeArray<TypeMemberNode>
    | Error of string

[<RequireQualifiedAccess>]
module Parse =
    val internal parser : Parser<ParsedNodeArray<TopLevelNode>, unit>

    /// <exception cref="T:System.ArgumentNullException"/>
    val fromStream : source: System.IO.Stream -> ParsedNodeArray<TopLevelNode>

[<RequireQualifiedAccess>]
module AnyTypeNode =
    val primitive : UByte.Format.Model.PrimitiveType -> AnyTypeNode
