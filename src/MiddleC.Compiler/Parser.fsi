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

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type AnyTypeNode =
    | Primitive of UByte.Format.Model.PrimitiveType
    | Array of ParsedNode<AnyTypeNode>
    //| ObjectReference of Choice<TypeNode, ParsedTypeIdentifier>

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

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ExpressionNode =
    | LiteralBool of bool
    | LiteralChar32 of uint32
    | LiteralU32 of uint32
    | LiteralS32 of int32
    /// Represents a local, parameter, field, or method call.
    | Symbol of ParsedNamespaceName * ParsedNodeArray<ParsedIdentifier> * arguments: ParsedNodeArray<ExpressionNode> voption
    | NewObject of ParsedNode<AnyTypeNode> * ParsedNode<ConstructionExpression>
    //| UnaryOperation
    | BinaryOperation of BinaryOperation * x: ParsedExpression * y: ParsedExpression

and [<RequireQualifiedAccess; NoComparison; NoEquality>] ConstructionExpression =
    | String of string
    | ArrayElements of elements: ParsedNodeArray<ExpressionNode>
    | ConstructorCall of arguments: ParsedNodeArray<ExpressionNode>

and ParsedExpression = ParsedNode<ExpressionNode>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type StatementNode =
    | If of condition: ParsedExpression * trueStatementNodes: ParsedNodeArray<StatementNode> *
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
    /// Indicates that the method is the entry point of the module, can only be applied to one method in an entire module.
    | Entrypoint
    | Instance
    | Private
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
