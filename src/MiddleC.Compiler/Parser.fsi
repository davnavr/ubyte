namespace MiddleC.Compiler.Parser

open System
open System.Collections.Immutable
open System.Diagnostics
open System.Runtime.CompilerServices

open FParsec

[<RequireQualifiedAccess; IsReadOnly; Struct; StructuralComparison; StructuralEquality; DebuggerDisplay("{ToString()}")>]
type ParsedIdentifier =
    internal
    | Identifier of string

    override ToString : unit -> string

    interface IEquatable<ParsedIdentifier>

[<IsReadOnly; Struct>]
type ParsedNode<'Content> =
    { Line: uint32
      Column: uint32
      Content: 'Content }

type IdentifierNode = ParsedNode<ParsedIdentifier>

type ParsedNodeArray<'Content> = ImmutableArray<ParsedNode<'Content>>

type ParsedNamespaceName = ParsedNodeArray<ParsedIdentifier>

[<RequireQualifiedAccess; IsReadOnly; Struct; NoComparison; CustomEquality; DebuggerDisplay("{ToString()}")>]
type TypeIdentifier =
    { Name: IdentifierNode
      Namespace: ParsedNamespaceName }

    override GetHashCode : unit -> int32

    override Equals : obj -> bool

    override ToString : unit -> string

    interface IEquatable<TypeIdentifier>

type ParsedTypeIdentifier = ParsedNode<TypeIdentifier>

[<RequireQualifiedAccess; NoComparison; NoEquality; DebuggerDisplay("{ToString()}")>]
type AnyTypeNode =
    | Primitive of UByte.Format.Model.PrimitiveType
    | Array of ParsedNode<AnyTypeNode>
    | Defined of ParsedTypeIdentifier
    | ObjectReference of ParsedNode<AnyTypeNode>

    override ToString : unit -> string

[<RequireQualifiedAccess; Struct; DebuggerDisplay("{ToString()}")>]
type BinaryOperation =
    | Multiplication
    | Division
    | Modulo
    | Addition
    | Subtraction
    //| BitwiseShiftLeft
    //| BitwiseShiftRight
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | IsEqual
    | IsNotEqual
    | BitwiseAnd
    | BitwiseXor
    | BitwiseOr
    | BooleanAnd
    | BooleanOr
    | Assignment

    override ToString : unit -> string

[<RequireQualifiedAccess; NoComparison; NoEquality; DebuggerDisplay("{ToString()}")>]
type ExpressionNode =
    | LiteralBool of bool
    | LiteralChar32 of uint32
    | LiteralU32 of uint32
    | LiteralS32 of int32
    | Local of name: IdentifierNode
    | NewObject of ParsedNode<AnyTypeNode> * ParsedNode<ConstructionExpression>
    //| UnaryOperation
    | BinaryOperation of BinaryOperation * x: ParsedExpression * y: ParsedExpression
    /// Represents a field access or method call.
    | MemberAccess of Choice<ParsedNamespaceName, ParsedExpression> * name: IdentifierNode *
        arguments: ParsedNodeArray<ExpressionNode> voption

    override ToString : unit -> string

and [<RequireQualifiedAccess; NoComparison; NoEquality; DebuggerDisplay("{ToString()}")>] ConstructionExpression =
    | String of string
    | ArrayElements of elements: ParsedNodeArray<ExpressionNode>
    | ConstructorCall of arguments: ParsedNodeArray<ExpressionNode>

    override ToString : unit -> string

and ParsedExpression = ParsedNode<ExpressionNode>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type StatementNode =
    | If of condition: ParsedExpression * trueStatementNodes: ParsedNodeArray<StatementNode> *
        additionalStatementNodes: ImmutableArray<ParsedExpression * ParsedNodeArray<StatementNode>> *
        falseStatementNodes: ParsedNodeArray<StatementNode>
    // An expression that is simply evaluated for side effects.
    | Expression of ParsedExpression
    | While of condition: ParsedExpression * body: ParsedNodeArray<StatementNode>
    | Goto of IdentifierNode
    | Label of IdentifierNode
    | LocalDeclaration of constant: bool * name: IdentifierNode * ParsedNode<AnyTypeNode> * value: ParsedExpression
    | Return of ParsedExpression
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
    | Abstract
    /// The method is the entry point of the module, can only be applied to one method in an entire module.
    | Entrypoint
    | Instance
    | Private
    /// The method can be used by importing modules.
    | Public
    | Virtual

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type FieldAttributeNode =
    /// The field value can be modified outside of a constructor or initializer.
    | Mutable
    | Private
    /// The field does not belong to any instance of the class.
    | Static

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ConstructorAttributeNode =
    | Public
    | Private

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type MethodBodyNode =
    | Defined of ParsedNodeArray<StatementNode>
    | External of ParsedNode<string> * library: ParsedNode<string>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type TypeMemberNode =
    | FieldDeclaration of name: IdentifierNode * attributes: ParsedNodeArray<FieldAttributeNode> *
        fieldType: ParsedNode<AnyTypeNode> * initialValue: ParsedExpression voption
    | MethodDeclaration of name: IdentifierNode * attributes: ParsedNodeArray<MethodAttributeNode> *
        parameters: ImmutableArray<IdentifierNode * ParsedNode<AnyTypeNode>> * returnTypes: ParsedNodeArray<AnyTypeNode> *
        body: MethodBodyNode
    //| InitializerDeclaration of body: ParsedNodeArray<StatementNode>
    | ConstructorDeclaration of attributes: ParsedNodeArray<ConstructorAttributeNode> *
        parameters: ImmutableArray<IdentifierNode * ParsedNode<AnyTypeNode>> * body: ParsedNodeArray<StatementNode>
    //| NestedType of 

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type TopLevelNode =
    | UsingNamespace of ParsedNamespaceName
    | NamespaceDeclaration of ParsedNamespaceName * ParsedNodeArray<TopLevelNode>
    | TypeDeclaration of name: IdentifierNode * ParsedNodeArray<TypeAttributeNode> * extends: ParsedNodeArray<TypeIdentifier> *
        members: ParsedNodeArray<TypeMemberNode>
    | Error of string

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ParsedFile =
    { Source: string
      Nodes: ParsedNodeArray<TopLevelNode> }

[<RequireQualifiedAccess>]
module Parse =
    val internal parser : Parser<ParsedNodeArray<TopLevelNode>, unit>

    /// <exception cref="T:System.ArgumentNullException"/>
    val fromPath : path: string -> ParsedFile

[<RequireQualifiedAccess>]
module AnyTypeNode =
    val primitive : UByte.Format.Model.PrimitiveType -> AnyTypeNode
