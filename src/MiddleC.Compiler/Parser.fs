namespace MiddleC.Compiler.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable

open FParsec

[<RequireQualifiedAccess; Struct; StructuralComparison; StructuralEquality>]
type ParsedIdentifier =
    internal
    | Identifier of string

    override this.ToString() = let (Identifier id) = this in id

[<Struct>]
type ParsedNode<'Content> =
    { Line: uint32
      Column: uint32
      Content: 'Content }

type IdentifierNode = ParsedNode<ParsedIdentifier>

type ParsedNodeArray<'Content> = ImmutableArray<ParsedNode<'Content>>

type ParsedNamespaceName = ParsedNodeArray<ParsedIdentifier>

[<RequireQualifiedAccess>]
type AnyTypeNode =
    | Primitive of UByte.Format.Model.PrimitiveType
    | Array of AnyTypeNode

[<RequireQualifiedAccess>]
type ExpressionNode =
    | LiteralCharacterArray of AnyTypeNode * elements: string
    | LiteralBool of bool
    | LiteralChar32 of uint32
    | LiteralU32 of uint32
    | LiteralS32 of int32

and ParsedExpression = ParsedNode<ExpressionNode>

[<RequireQualifiedAccess>]
type StatementNode =
    | If of condition: ParsedExpression * trueStatementNodes: ParsedNodeArray<StatementNode> *
        falseStatementNodes: ParsedNodeArray<StatementNode>
    | Expression of ParsedExpression
    | While of condition: ParsedExpression * body: ParsedNodeArray<StatementNode>
    | Goto of IdentifierNode
    | Label of IdentifierNode
    | Empty

[<RequireQualifiedAccess>]
type TypeAttributeNode =
    | Abstract
    | Inheritable

[<RequireQualifiedAccess>]
type MethodAttributeNode =
    | Entrypoint
    | Instance
    | Abstract
    | Virtual

[<RequireQualifiedAccess>]
type MethodBodyNode =
    | Defined of ParsedNodeArray<StatementNode>
    | External of ParsedNode<string> * library: ParsedNode<string>

[<RequireQualifiedAccess>]
type TypeMemberNode =
    | MethodDeclaration of name: IdentifierNode * ParsedNodeArray<MethodAttributeNode> *
        parameters: ImmutableArray<IdentifierNode * ParsedNode<AnyTypeNode>> * ParsedNodeArray<AnyTypeNode> * MethodBodyNode

[<RequireQualifiedAccess>]
type TopLevelNode =
    | UsingNamespace of ParsedNamespaceName
    | NamespaceDeclaration of ParsedNamespaceName * ParsedNodeArray<TopLevelNode>
    | TypeDeclaration of name: IdentifierNode * ParsedNodeArray<TypeAttributeNode> * ParsedNodeArray<TypeMemberNode>
    | Error of string

[<RequireQualifiedAccess>]
module AnyTypeNode =
    let private primitives = Dictionary()

    let primitive ty =
        match primitives.TryGetValue ty with
        | true, existing -> existing
        | false, _ ->
            let node = AnyTypeNode.Primitive ty
            primitives.[ty] <- node
            node

module private CollectionParsers =
    [<RequireQualifiedAccess>]
    module ImmutableArray =
        let inline foldState (builder: ImmutableArray<_>.Builder) element =
            builder.Add element
            builder

        let inline stateFromFirstElement element = foldState (ImmutableArray.CreateBuilder()) element

        let inline resultFromState (builder: ImmutableArray<_>.Builder) = builder.ToImmutable()

        let inline resultForEmptySequence () = ImmutableArray.Empty

        let many p =
            Inline.Many (
                stateFromFirstElement,
                foldState,
                resultFromState,
                p,
                resultForEmptySequence = resultForEmptySequence
            )

        let private sepByCommon p sep hasFirstElement =
            Inline.SepBy (
                stateFromFirstElement,
                (fun builder _ element -> foldState builder element),
                resultFromState,
                p,
                sep,
                ?firstElementParser = (if hasFirstElement then Some p else None),
                resultForEmptySequence = resultForEmptySequence
            )


        let sepBy p sep = sepByCommon p sep false
        let sepBy1 p sep = sepByCommon p sep true

[<RequireQualifiedAccess>]
module Parse =
    let semicolon = skipChar ';'
    let comma = skipChar ','

    let betweenCurlyBrackets inner = between (skipChar '{') (skipChar '}') inner

    let betweenParenthesis inner = between (skipChar '(') (skipChar ')') inner

    let whitespace : Parser<unit, _> = choice [
        spaces
        skipString "//" .>> skipRestOfLine true <?> "single-line comment"
        //between (skipString "/*") (skipString "*/")
    ]

    let withNodeContent (parser: Parser<'Content, _>) : Parser<_, unit> =
        fun stream ->
            let line = uint32 stream.Line
            let column = uint32 stream.Column
            let result = parser stream
            if result.Status = ReplyStatus.Ok then
                Reply { ParsedNode.Content = result.Result; Line = line; Column = column }
            else
                Reply(result.Status, result.Error)

    let validIdentifierNode : Parser<IdentifierNode, unit> =
        let isValidFirstCharacter c = isLetter c || c = '_'
        many1Satisfy2
            isValidFirstCharacter
            (fun c -> isValidFirstCharacter c || isDigit c)
        |>> ParsedIdentifier.Identifier
        |> withNodeContent

    let namespaceNameNode : Parser<ParsedNamespaceName, unit> =
        CollectionParsers.ImmutableArray.sepBy1 validIdentifierNode (skipString "::")

    let private topLevelNode, topLevelNodeRef : Parser<ParsedNode<TopLevelNode>, _> * _ = createParserForwardedToRef()

    let attributes attributes : Parser<ParsedNodeArray<_>, _> =
        let parsers =
            Array.map
                (fun struct(name, attribute) -> skipString name >>% attribute)
                attributes

        choice parsers
        .>> whitespace
        |> withNodeContent
        |> CollectionParsers.ImmutableArray.many

    let anyTypeNode : Parser<ParsedNode<AnyTypeNode>, unit> =
        let primitiveTypeNode =
            Array.map
                (fun struct(name, ty) -> skipString name >>% AnyTypeNode.Primitive ty)
                [|
                    "s32", UByte.Format.Model.PrimitiveType.S32
                    "char32", UByte.Format.Model.PrimitiveType.Char32
                |]
            |> choice

        let typeNodeModifiers = choice [
            skipString "[]" >>% AnyTypeNode.Array
        ]

        let baseTypeNodes = choice [
            primitiveTypeNode
        ]

        pipe2
            (baseTypeNodes .>> whitespace)
            (many (typeNodeModifiers .>> whitespace))
            (fun btype modifiers -> (List.fold (fun mf m -> fun ty -> m(mf ty)) id modifiers) btype)
        |> withNodeContent

    let expressionNode : Parser<ParsedNode<ExpressionNode>, _> =
        // TODO: If integer literal is out of range, generate an error node.
        let unsignedIntegerLiteralOptions = NumberLiteralOptions.AllowBinary ||| NumberLiteralOptions.AllowHexadecimal
        let signedIntegerLiteralOptions = unsignedIntegerLiteralOptions ||| NumberLiteralOptions.AllowMinusSign
        let numberlit options parser node =
            numberLiteral options String.Empty |>> fun literal -> node(parser literal.String)

        whitespace
        >>. choice
            [
                skipString "true" >>% ExpressionNode.LiteralBool true
                skipString "false" >>% ExpressionNode.LiteralBool false
                numberlit signedIntegerLiteralOptions Int32.Parse ExpressionNode.LiteralS32
                numberlit unsignedIntegerLiteralOptions UInt32.Parse ExpressionNode.LiteralU32
            ]
        //|> errorNodeHandler TopLevelNode.Error
        |> withNodeContent
        .>> whitespace

    let block, blockRef : Parser<ParsedNodeArray<StatementNode>, unit> * _ = createParserForwardedToRef()

    let statementNode : Parser<ParsedNode<StatementNode>, _> =
        whitespace
        >>. choice
            [
                skipString "if"
                >>. whitespace
                >>. tuple3
                    (betweenParenthesis expressionNode .>> whitespace)
                    (block .>> whitespace)
                    (choice [
                        skipString "else" >>. whitespace >>. block
                        preturn ImmutableArray.Empty
                    ])
                |>> StatementNode.If

                // TODO: How to parse else if?

                expressionNode |>> StatementNode.Expression
                followedBy semicolon >>% StatementNode.Empty
            ]
        .>> whitespace
        .>> semicolon
        .>> whitespace
        |> withNodeContent

    blockRef.Value <- betweenCurlyBrackets(CollectionParsers.ImmutableArray.many statementNode)

    let typeMemberNode : Parser<ParsedNode<TypeMemberNode>, unit> =
        let methodParameterNodes =
            CollectionParsers.ImmutableArray.sepBy
                (whitespace >>. validIdentifierNode .>> whitespace .>>. anyTypeNode .>> whitespace)
                comma
            |> betweenParenthesis

        let methodReturnTypes =
            CollectionParsers.ImmutableArray.sepBy (whitespace >>. anyTypeNode .>> whitespace) comma |> betweenParenthesis

        let methodBodyNode : Parser<MethodBodyNode, _> =
            choice [
                //skipString "external" >>.

                block |>> MethodBodyNode.Defined
            ]

        whitespace
        >>. choice
            [
                skipString "method"
                >>. whitespace
                >>. tuple5
                    (validIdentifierNode .>> whitespace)
                    (attributes [|
                        "abstract", MethodAttributeNode.Abstract
                        "entrypoint", MethodAttributeNode.Entrypoint
                        "instance", MethodAttributeNode.Instance
                        "virtual", MethodAttributeNode.Virtual
                    |])
                    (whitespace >>. methodParameterNodes .>> whitespace .>> skipString "->" .>> whitespace)
                    (methodReturnTypes .>> whitespace)
                    methodBodyNode
                |>> TypeMemberNode.MethodDeclaration
            ]
        .>> whitespace
        |> withNodeContent

    topLevelNodeRef.Value <-
        whitespace
        >>. choice [
            skipString "namespace"
            >>. whitespace
            >>. namespaceNameNode
            .>> whitespace
            .>>. betweenCurlyBrackets (CollectionParsers.ImmutableArray.many topLevelNode)
            |>> TopLevelNode.NamespaceDeclaration

            skipString "using"
            >>. whitespace
            >>. namespaceNameNode
            .>> whitespace
            .>> semicolon
            |>> TopLevelNode.UsingNamespace

            skipString "type"
            >>. whitespace
            >>. tuple3
                (validIdentifierNode .>> whitespace)
                (attributes [|
                    "abstract", TypeAttributeNode.Abstract
                    "inheritable", TypeAttributeNode.Inheritable
                |])
                (whitespace >>. betweenCurlyBrackets (CollectionParsers.ImmutableArray.many typeMemberNode))
            |>> TopLevelNode.TypeDeclaration
        ]
        .>> whitespace
        //|> errorNodeHandler TopLevelNode.Error
        |> withNodeContent

    let parser = CollectionParsers.ImmutableArray.many topLevelNode .>> eof

    let private defaultStreamEncoding = System.Text.Encoding.UTF8

    let fromStream source =
        if isNull source then nullArg (nameof source)
        match runParserOnStream parser () String.Empty source defaultStreamEncoding with
        | Success(nodes, (), _) -> nodes
        | Failure(message, _, ()) -> invalidOp message
