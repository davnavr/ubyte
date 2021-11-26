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

type ParsedNamespaceName = ParsedNodeArray<ParsedIdentifier> // TODO: Create wrapper for ParsedNamespaceName to override equality implementation.

[<RequireQualifiedAccess; Struct; NoComparison; CustomEquality>]
type TypeIdentifier =
    { Name: IdentifierNode; Namespace: ParsedNamespaceName }

    override id.GetHashCode() =
        let mutable hash = HashCode()
        hash.Add id.Name.Content
        for ns in id.Namespace do hash.Add ns.Content
        hash.ToHashCode()

    override this.ToString() =
        let sb = System.Text.StringBuilder()
        if not this.Namespace.IsDefaultOrEmpty then
            for ns in this.Namespace do
                sb.Append(ns.Content.ToString()).Append("::") |> ignore
        sb.Append(this.Name.Content.ToString()).ToString()

    override id.Equals o =
        match o with
        | :? TypeIdentifier as other -> (id :> IEquatable<TypeIdentifier>).Equals other
        | _ -> false

    interface IEquatable<TypeIdentifier> with /// Might be better if this was in an IEqualityComparer implementation instead.
        member id.Equals other =
            let mutable equals = id.Namespace.Length = other.Namespace.Length
            let mutable i = 0
            while equals && i < id.Namespace.Length do
                equals <- id.Namespace.[i].Content = other.Namespace.[i].Content
                i <- i + 1
            equals && id.Name.Content = other.Name.Content

type ParsedTypeIdentifier = ParsedNode<TypeIdentifier>

[<RequireQualifiedAccess>]
type AnyTypeNode =
    | Primitive of UByte.Format.Model.PrimitiveType
    | Array of ParsedNode<AnyTypeNode>

[<RequireQualifiedAccess>]
type ExpressionNode =
    | LiteralBool of bool
    | LiteralChar32 of uint32
    | LiteralU32 of uint32
    | LiteralS32 of int32
    | Symbol of ParsedNamespaceName * ParsedNodeArray<ParsedIdentifier> * arguments: ParsedNodeArray<ExpressionNode> voption
    | NewObject of ParsedNode<AnyTypeNode> * ParsedNode<ConstructionExpression>

and [<RequireQualifiedAccess>] ConstructionExpression =
    | String of string
    | ArrayElements of elements: ParsedNodeArray<ExpressionNode>
    | ConstructorCall of arguments: ParsedNodeArray<ExpressionNode>

and ParsedExpression = ParsedNode<ExpressionNode>

[<RequireQualifiedAccess>]
type StatementNode =
    | If of condition: ParsedExpression * trueStatementNodes: ParsedNodeArray<StatementNode> *
        falseStatementNodes: ParsedNodeArray<StatementNode>
    | Expression of ParsedExpression
    | While of condition: ParsedExpression * body: ParsedNodeArray<StatementNode>
    | Goto of IdentifierNode
    | Label of IdentifierNode
    | LocalDeclaration of constant: bool * name: IdentifierNode * ParsedNode<AnyTypeNode> * value: ParsedExpression
    | Return of ParsedExpression
    | Empty

[<RequireQualifiedAccess>]
type TypeAttributeNode =
    | Abstract
    | Inheritable

[<RequireQualifiedAccess>]
type MethodAttributeNode =
    | Abstract
    | Entrypoint
    | Instance
    | Private
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
    | TypeDeclaration of name: IdentifierNode * ParsedNodeArray<TypeAttributeNode> * extends: ParsedNodeArray<TypeIdentifier> *
        members: ParsedNodeArray<TypeMemberNode>
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
        let inline private foldState (builder: ImmutableArray<_>.Builder) element =
            builder.Add element
            builder

        let inline private stateFromFirstElement element = foldState (ImmutableArray.CreateBuilder()) element

        let inline private resultFromState (builder: ImmutableArray<_>.Builder) = builder.ToImmutable()

        let inline private resultForEmptySequence () = ImmutableArray.Empty

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
    module Dictionary =
        let inline private foldState (lookup: Dictionary<_, _>) (KeyValue(key, value)) =
            lookup.Add(key, value)
            lookup

        let inline private stateFromFirstElement pair = foldState (Dictionary()) pair

        let inline private resultForEmptySequence () = Dictionary()

        let private sepByCommon p sep hasFirstElement =
            Inline.SepBy (
                stateFromFirstElement,
                (fun builder _ element -> foldState builder element),
                id,
                p,
                sep,
                ?firstElementParser = (if hasFirstElement then Some p else None),
                resultForEmptySequence = resultForEmptySequence
            )

        let sepBy p sep = sepByCommon p sep false
        let sepBy1 p sep = sepByCommon p sep true

[<RequireQualifiedAccess>]
type ParsedFile = { Source: string; Nodes: ParsedNodeArray<TopLevelNode> }

[<RequireQualifiedAccess>]
module Parse =
    let private semicolon = skipChar ';'
    let private comma = skipChar ','
    let private equals = skipChar '='
    let private dquote = skipChar '\"'
    let private period = skipChar '.'
    let private namespaceNameSeparator = skipString "::"

    let private vopt p = choice [ p |>> ValueSome; preturn ValueNone ]

    let private betweenCurlyBrackets inner = between (skipChar '{') (skipChar '}') inner

    let private betweenParenthesis inner = between (skipChar '(') (skipChar ')') inner

    let private whitespace : Parser<unit, _> = choice [
        spaces
        skipString "//" .>> skipRestOfLine true <?> "single-line comment"
        //between (skipString "/*") (skipString "*/")
    ]

    let private withNodeContent (parser: Parser<'Content, _>) : Parser<_, unit> =
        fun stream ->
            let line = uint32 stream.Line
            let column = uint32 stream.Column
            let result = parser stream
            if result.Status = ReplyStatus.Ok then
                Reply { ParsedNode.Content = result.Result; Line = line; Column = column }
            else
                Reply(result.Status, result.Error)

    let private validIdentifierNode : Parser<IdentifierNode, unit> =
        let isValidFirstCharacter c = isLetter c || c = '_'
        many1Satisfy2
            isValidFirstCharacter
            (fun c -> isValidFirstCharacter c || isDigit c)
        |>> ParsedIdentifier.Identifier
        |> withNodeContent

    let private namespaceNameNode : Parser<ParsedNamespaceName, unit> =
        CollectionParsers.ImmutableArray.sepBy1 validIdentifierNode namespaceNameSeparator

    let private typeIdentifierNode : Parser<ParsedTypeIdentifier, unit> =
        choice [
            pipe2
                (namespaceNameNode .>> namespaceNameSeparator)
                validIdentifierNode
                (fun ns name -> { TypeIdentifier.Name = name; TypeIdentifier.Namespace = ns })
            |> attempt

            validIdentifierNode |>> fun name -> { TypeIdentifier.Name = name; TypeIdentifier.Namespace = ImmutableArray.Empty }
        ]
        |> withNodeContent

    let private topLevelNode, private topLevelNodeRef : Parser<ParsedNode<TopLevelNode>, _> * _ = createParserForwardedToRef()

    let private attributes attributes : Parser<ParsedNodeArray<_>, _> =
        let parsers =
            Array.map
                (fun struct(name, attribute) -> stringReturn name attribute)
                attributes

        choice parsers
        .>> whitespace
        |> withNodeContent
        |> CollectionParsers.ImmutableArray.many

    let private anyTypeNode : Parser<ParsedNode<AnyTypeNode>, unit> =
        let primitiveTypeNode =
            Array.map
                (fun struct(name, ty) -> stringReturn name (AnyTypeNode.Primitive ty))
                [|
                    "s32", UByte.Format.Model.PrimitiveType.S32
                    "char32", UByte.Format.Model.PrimitiveType.Char32
                |]
            |> choice

        let parser = OperatorPrecedenceParser<ParsedNode<AnyTypeNode>, unit, _>()

        parser.TermParser <-
            choice [|
                primitiveTypeNode
            |]
            |> withNodeContent
            .>> whitespace

        let typeNodeMapping mapping (node: ParsedNode<AnyTypeNode>) =
            { node with Content = mapping node }

        PostfixOperator<_, unit, unit> (
            "[]",
            whitespace,
            precedence = 1,
            isAssociative = true,
            mapping = typeNodeMapping AnyTypeNode.Array
        )
        |> parser.AddOperator

        parser.ExpressionParser

    let private stringlit : Parser<string, _> =
        let escape =
            anyChar |>> function
            | 'n' -> "\n"
            | 'r' -> "\r"
            | 't' -> "\t"
            | c -> string c

        stringsSepBy
            (manySatisfy (isNoneOf [| '\"'; '\\'; '\n' |]))
            (skipChar '\\' >>. escape)
        |> between dquote dquote

    let private expression, private expressionRef: Parser<ParsedExpression, _> * _ = createParserForwardedToRef()

    expressionRef.Value <-
        // TODO: If integer literal is out of range, generate an error node.
        let unsignedIntegerLiteralOptions = NumberLiteralOptions.AllowBinary ||| NumberLiteralOptions.AllowHexadecimal
        let signedIntegerLiteralOptions = unsignedIntegerLiteralOptions ||| NumberLiteralOptions.AllowMinusSign
        let numberlit options parser node =
            numberLiteral options String.Empty |>> fun literal -> node(parser literal.String)

        whitespace
        >>. choice
            [|
                stringReturn "true" (ExpressionNode.LiteralBool true)
                stringReturn "false" (ExpressionNode.LiteralBool false)
                numberlit signedIntegerLiteralOptions Int32.Parse ExpressionNode.LiteralS32
                numberlit unsignedIntegerLiteralOptions UInt32.Parse ExpressionNode.LiteralU32

                let arguments =
                    betweenParenthesis(CollectionParsers.ImmutableArray.sepBy (whitespace >>. expression .>> whitespace) comma)

                skipString "new"
                >>. whitespace
                >>. anyTypeNode
                .>> whitespace
                .>>. (choice
                    [
                        stringlit |>> ConstructionExpression.String
                        arguments |>> ConstructionExpression.ConstructorCall

                        CollectionParsers.ImmutableArray.sepBy1 (whitespace >>. expression .>> whitespace) comma
                        |> betweenCurlyBrackets
                        |>> ConstructionExpression.ArrayElements
                    ]
                    |> withNodeContent)
                |>> ExpressionNode.NewObject

                tuple3
                    (CollectionParsers.ImmutableArray.many(attempt (validIdentifierNode .>> namespaceNameSeparator)))
                    (CollectionParsers.ImmutableArray.sepBy1 validIdentifierNode period .>> whitespace)
                    (vopt arguments)
                |>> ExpressionNode.Symbol
            |]
        //|> errorNodeHandler TopLevelNode.Error
        |> withNodeContent
        .>> whitespace

    let private block, private blockRef : Parser<ParsedNodeArray<StatementNode>, unit> * _ = createParserForwardedToRef()

    let private statement : Parser<ParsedNode<StatementNode>, _> =
        whitespace
        >>. choice
            [|
                skipString "if"
                >>. whitespace
                >>. tuple3
                    (betweenParenthesis expression .>> whitespace)
                    (block .>> whitespace)
                    (choice [|
                        skipString "else" >>. whitespace >>. block
                        preturn ImmutableArray.Empty
                    |])
                |>> StatementNode.If

                // TODO: How to parse else if?

                tuple4
                    (choice [ stringReturn "let" true; stringReturn "var" false ])
                    (whitespace >>. validIdentifierNode)
                    (whitespace >>. anyTypeNode .>> whitespace)
                    (equals >>. whitespace >>. expression)
                |>> StatementNode.LocalDeclaration

                skipString "return" >>. whitespace >>. expression |>> StatementNode.Return

                // All parsers that take a keyword must be above this to ensure keywords aren't interpreted as names.
                expression |>> StatementNode.Expression
                followedBy semicolon >>% StatementNode.Empty
            |]
        .>> whitespace
        .>> semicolon
        .>> whitespace
        |> withNodeContent

    blockRef.Value <- betweenCurlyBrackets(CollectionParsers.ImmutableArray.many statement)

    let private typeMemberNode : Parser<ParsedNode<TypeMemberNode>, unit> =
        let methodParameterNodes =
            CollectionParsers.ImmutableArray.sepBy
                (whitespace >>. validIdentifierNode .>> whitespace .>>. anyTypeNode .>> whitespace)
                comma
            |> betweenParenthesis

        let methodReturnTypes =
            CollectionParsers.ImmutableArray.sepBy (whitespace >>. anyTypeNode .>> whitespace) comma |> betweenParenthesis

        let methodBodyNode : Parser<MethodBodyNode, _> =
            choice [|
                let externalMethodProperties =
                    CollectionParsers.Dictionary.sepBy
                        (whitespace
                        >>. pipe2
                            (many1Chars letter .>> whitespace .>> equals .>> whitespace)
                            (withNodeContent stringlit)
                            (fun key value -> KeyValuePair(key, value))
                        .>> whitespace)
                        comma

                skipString "external"
                >>. whitespace
                >>. betweenCurlyBrackets externalMethodProperties
                |>> fun keys -> MethodBodyNode.External(keys.["entry"], keys.["library"])

                block |>> MethodBodyNode.Defined
            |]

        whitespace
        >>. choice
            [|
                skipString "method"
                >>. whitespace
                >>. tuple5
                    (validIdentifierNode .>> whitespace)
                    (attributes [|
                        "abstract", MethodAttributeNode.Abstract
                        "entrypoint", MethodAttributeNode.Entrypoint
                        "instance", MethodAttributeNode.Instance
                        "private", MethodAttributeNode.Private
                        "virtual", MethodAttributeNode.Virtual
                    |])
                    (whitespace >>. methodParameterNodes .>> whitespace .>> skipString "->" .>> whitespace)
                    (methodReturnTypes .>> whitespace)
                    methodBodyNode
                |>> TypeMemberNode.MethodDeclaration
            |]
        .>> whitespace
        |> withNodeContent

    topLevelNodeRef.Value <-
        whitespace
        >>. choice [|
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
            >>. tuple4
                (validIdentifierNode .>> whitespace)
                (attributes [|
                    "abstract", TypeAttributeNode.Abstract
                    "inheritable", TypeAttributeNode.Inheritable
                |])
                (whitespace >>. choice [|
                    semicolon >>. whitespace >>. CollectionParsers.ImmutableArray.sepBy1 typeIdentifierNode comma
                    preturn ImmutableArray.Empty
                |])
                (whitespace >>. betweenCurlyBrackets (CollectionParsers.ImmutableArray.many typeMemberNode))
            |>> TopLevelNode.TypeDeclaration
        |]
        .>> whitespace
        //|> errorNodeHandler TopLevelNode.Error
        |> withNodeContent

    let parser = CollectionParsers.ImmutableArray.many topLevelNode .>> eof

    let private defaultStreamEncoding = System.Text.Encoding.UTF8

    let fromPath path =
        if isNull path then nullArg (nameof path)
        match runParserOnFile parser () path defaultStreamEncoding with
        | Success(nodes, (), _) ->
            { ParsedFile.Nodes = nodes
              ParsedFile.Source = path }
        | Failure(message, _, ()) -> invalidOp message
