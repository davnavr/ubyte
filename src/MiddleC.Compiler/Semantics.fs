﻿namespace MiddleC.Compiler.Semantics

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open MiddleC.Compiler.Parser

open UByte.Format

[<Struct; NoComparison; StructuralEquality>]
type FullNamespaceName =
    internal
    | FullNamespaceName of ParsedNamespaceName

    override this.ToString() =
        let (FullNamespaceName names) = this
        let sb = System.Text.StringBuilder()
        for i = 0 to names.Length - 1 do
            if i > 0 then sb.Append "::" |> ignore
            sb.Append names.[i].Content |> ignore
        sb.ToString()

[<RequireQualifiedAccess>]
module FullNamespaceName =
    let empty = FullNamespaceName ImmutableArray.Empty

    let append (nested: ParsedNamespaceName) (FullNamespaceName parent) =
        FullNamespaceName(parent.AddRange nested)

    let toParsedName (FullNamespaceName ns) = ns

    let toStrings (FullNamespaceName ns) =
        if not ns.IsDefaultOrEmpty then
            let mutable names = Array.zeroCreate ns.Length
            for i = 0 to names.Length - 1 do names.[i] <- ns.[i].Content.ToString()
            Unsafe.As<string[], ImmutableArray<string>> &names
        else
            ImmutableArray.Empty

[<Struct; NoComparison; StructuralEquality>]
type FullTypeIdentifier =
    internal
    | FullTypeIdentifier of TypeIdentifier

    member this.Name = let (FullTypeIdentifier { TypeIdentifier.Name = name }) = this in name

    member this.Namespace = let (FullTypeIdentifier { TypeIdentifier.Namespace = ns }) = this in FullNamespaceName ns

    override this.ToString() = let (FullTypeIdentifier id) = this in id.ToString()

[<RequireQualifiedAccess>]
module FullTypeIdentifier =
    let inline create tname tnamespace =
        { TypeIdentifier.Name = tname
          TypeIdentifier.Namespace = tnamespace }
        |> FullTypeIdentifier

    let ofTypeIdentifier (FullNamespaceName fnamespace) (id: inref<TypeIdentifier>) =
        create id.Name (fnamespace.AddRange id.Namespace)

type NamedType = Choice<CheckedTypeDefinition, UByte.Resolver.ResolvedTypeDefinition>

and [<NoComparison; StructuralEquality>] CheckedValueType =
    | Primitive of Model.PrimitiveType

    override this.ToString() =
        match this with
        | Primitive pt -> pt.ToString().ToLowerInvariant()

and [<RequireQualifiedAccess; NoComparison; StructuralEquality>] CheckedReferenceType =
    | Any
    | Array of CheckedElementType

    override this.ToString() =
        match this with
        | Any -> "anyobj"
        | Array etype -> etype.ToString() + "[]"

and [<RequireQualifiedAccess; NoComparison; StructuralEquality>] CheckedElementType =
    | ValueType of CheckedValueType
    | ReferenceType of CheckedReferenceType

    override this.ToString() =
        match this with
        | ValueType vt -> vt.ToString()
        | ReferenceType rt -> rt.ToString()

and [<RequireQualifiedAccess; NoComparison; StructuralEquality>] CheckedType =
    | ValueType of CheckedValueType
    | ReferenceType of CheckedReferenceType
    | Void

    override this.ToString() =
        match this with
        | ValueType vt -> vt.ToString()
        | ReferenceType rt -> rt.ToString()
        | Void -> "()"

and [<RequireQualifiedAccess>] CheckedExpression =
    | ArrayLengthAccess of TypedExpression
    | BinaryOperation of BinaryOperation * x: TypedExpression * y: TypedExpression
    | LiteralBoolean of bool
    | LiteralSignedInteger of int64
    | LiteralUnsignedInteger of uint64
    | Local of ParsedIdentifier
    | MethodCall of NamedMethod * arguments: ImmutableArray<TypedExpression>
    | NewArray of CheckedElementType * elements: ImmutableArray<TypedExpression>
    //| Nothing

    override this.ToString() =
        match this with
        | ArrayLengthAccess array -> array.ToString() + ".length"
        | BinaryOperation(op, x, y) -> "(" + x.ToString() + " " + op.ToString() + " " + y.ToString() + ")"
        | LiteralBoolean true -> "true"
        | LiteralBoolean false -> "false"
        | LiteralSignedInteger i -> string i
        | LiteralUnsignedInteger i -> string i
        | Local name -> name.ToString()
        | MethodCall(method, arguments) ->
            let methodName =
                match method with
                | Choice1Of2 defined -> defined.ToString()
                | Choice2Of2 imported -> imported.ToString()

            System.Text.StringBuilder(methodName).Append('(').AppendJoin(", ", arguments).Append(')').ToString()
        | NewArray(etype, elements) ->
            System.Text.StringBuilder("new ").Append(etype.ToString()).Append("[] { ").AppendJoin(", ", elements).Append(" }")
                .ToString()
        //| Nothing -> "()"

and TypedExpression =
    { Expression: CheckedExpression
      Type: CheckedType
      Node: ParsedNode<ExpressionNode> }

    override this.ToString() =
        match this with
        | { Expression = CheckedExpression.LiteralBoolean _ | CheckedExpression.NewArray _ | CheckedExpression.MethodCall _ | CheckedExpression.Local _ | CheckedExpression.ArrayLengthAccess _ | CheckedExpression.BinaryOperation _ as expr } ->
            expr.ToString() // add casting syntax?
        | { Expression = CheckedExpression.LiteralSignedInteger _ | CheckedExpression.LiteralUnsignedInteger _ as expr } ->
            match this.Type with
            | CheckedType.ValueType(CheckedValueType.Primitive prim) ->
                expr.ToString() // + prefix
            | _ ->
                "figure out casting syntax (" + expr.ToString() + ")"

and [<RequireQualifiedAccess>] CheckedStatement =
    | Expression of TypedExpression
    | If of condition: TypedExpression * thenBlockStatements: ImmutableArray<CheckedStatement> *
        elseBlockStatements: ImmutableArray<CheckedStatement>
    | LocalDeclaration of constant: bool * name: IdentifierNode * CheckedType(* CheckedLocal *) * value: TypedExpression
    | Return of ImmutableArray<TypedExpression>
    | While of condition: TypedExpression * body: ImmutableArray<CheckedStatement>
    | Empty

    override this.ToString() =
        match this with
        | Expression expression -> expression.ToString() + ";"
        | If(condition, thenBlockStatements, elseBlockStatements) ->
            let sb = System.Text.StringBuilder("if (").Append(condition.ToString()).AppendLine(") {")

            for statement in thenBlockStatements do
                sb.Append('\t').Append(statement.ToString()).AppendLine(";") |> ignore
            sb.Append '}' |> ignore

            if not elseBlockStatements.IsDefaultOrEmpty then
                sb.AppendLine(" else {") |> ignore
                for statement in elseBlockStatements do
                    sb.Append('\t').Append(statement.ToString()).AppendLine(";") |> ignore
                sb.Append '}' |> ignore

            sb.ToString()
        | LocalDeclaration(constant, name, ty, value) ->
            System.Text.StringBuilder(if constant then "let" else "var").Append(' ') .Append(name.ToString()).Append(' ')
                .Append(ty.ToString()).Append(" = ").Append(value.ToString()).Append(';').ToString()
        | Return value when value.IsDefaultOrEmpty -> "return;"
        | Return values -> System.Text.StringBuilder("return ").AppendJoin(", ", values).Append(';').ToString()
        | While(condition, statements) ->
            let sb = System.Text.StringBuilder("while (").Append(condition.ToString()).AppendLine(") {")
            for statement in statements do
                sb.Append('\t').Append(statement.ToString()).AppendLine(";") |> ignore
            sb.Append('}').ToString()
        | Empty -> ";"

and [<RequireQualifiedAccess>] CheckedLocal =
    { Name: IdentifierNode
      NestingLevel: uint32
      Type: CheckedType }

and [<RequireQualifiedAccess>] CheckedParameter =
    { Name: IdentifierNode
      Type: CheckedType }

    override this.ToString() = this.Name.Content.ToString() + " " + this.Type.ToString()

and [<RequireQualifiedAccess>] CheckedMethodBody =
    | Defined of ImmutableArray<CheckedStatement>
    | External of string * library: string

and [<RequireQualifiedAccess; Struct; NoComparison; StructuralEquality>] CheckedMethodSignature =
    { ParameterTypes: ImmutableArray<CheckedType>
      ReturnTypes: ImmutableArray<CheckedType> }

and [<Sealed>] CheckedMethod
    (
        owner: CheckedTypeDefinition,
        name: IdentifierNode,
        attributes: ParsedNodeArray<MethodAttributeNode>,
        methodParameterNodes: ImmutableArray<IdentifierNode * ParsedNode<AnyTypeNode>>,
        returnTypeNodes: ParsedNodeArray<AnyTypeNode>,
        methodBodyNode: MethodBodyNode
    )
    =
    let mutable flags = Unchecked.defaultof<Model.MethodFlags>
    let mutable visibility = Model.VisibilityFlags.Unspecified
    let mutable parameters = ImmutableArray<CheckedParameter>.Empty
    let mutable parameterTypes = ImmutableArray<CheckedType>.Empty
    let mutable returns = ImmutableArray<CheckedType>.Empty
    let mutable body = Unchecked.defaultof<CheckedMethodBody>

    member _.DeclaringType = owner
    member _.Name = name
    member _.AttributeNodes = attributes
    member _.ParameterNodes = methodParameterNodes
    member _.ReturnTypeNodes = returnTypeNodes
    member _.BodyNode = methodBodyNode
    member _.Visibility with get() = visibility and set value = visibility <- value
    member _.Flags with get() = flags and set value = flags <- value

    member _.Parameters
        with get() = parameters
        and set value =
            parameters <- value
            parameterTypes <- ImmutableArray.CreateRange(Seq.map (fun { CheckedParameter.Type = ty } -> ty) parameters)

    member _.ReturnTypes with get() = returns and set value = returns <- value
    member _.Signature = { CheckedMethodSignature.ReturnTypes = returns; CheckedMethodSignature.ParameterTypes = parameterTypes }
    member _.Body with get() = body and set value = body <- value // TODO: Make available dictionary of all locals.

    override this.ToString() =
        System.Text.StringBuilder(this.DeclaringType.Identifier.ToString()).Append("::").Append(this.Name.Content.ToString())
            .Append('(')
            .AppendJoin(", ", this.Parameters)
            .Append(')')
            .ToString()

and NamedMethod = Choice<CheckedMethod, UByte.Resolver.ResolvedMethod>

and [<Sealed>] CheckedField
    (
        owner: CheckedTypeDefinition,
        name: IdentifierNode,
        attributes: ParsedNodeArray<FieldAttributeNode>
    )
    =
    let mutable flags = Unchecked.defaultof<Model.FieldFlags>
    let mutable visibility = Model.VisibilityFlags.Unspecified
    let mutable fieldValueType = CheckedType.Void
    let mutable initialFieldValue: TypedExpression voption = ValueNone

    member _.DeclaringType = owner
    member _.Name = name
    member _.AttributeNodes = attributes
    member _.Flags with get() = flags and set value = flags <- value
    member _.Visibility with get() = visibility and set value = visibility <- value
    member _.FieldType with get() = fieldValueType and set value = fieldValueType <- value
    member _.InitialValue with get() = initialFieldValue

and [<Sealed>] CheckedTypeDefinition
    (
        identifier: FullTypeIdentifier,
        source: ParsedFile,
        typeAttributeNodes: ParsedNodeArray<TypeAttributeNode>,
        inheritedTypeNodes: ParsedNodeArray<TypeIdentifier>,
        typeMemberNodes: ParsedNodeArray<TypeMemberNode>,
        usedNamespaceDeclarations: ImmutableArray<FullNamespaceName>
    )
    =
    let mutable flags = Unchecked.defaultof<Model.TypeDefinitionFlags>
    let mutable inheritedTypeDefinitions = ImmutableArray.Empty
    let mutable methods = ImmutableArray<CheckedMethod>.Empty
    let mutable fields = ImmutableArray<CheckedField>.Empty

    member _.Identifier = identifier
    member _.Source = source
    member _.AttributeNodes = typeAttributeNodes
    member _.InheritanceNodes = inheritedTypeNodes
    member _.MemberNodes = typeMemberNodes
    /// A list of all namespaces that are in scope.
    member _.UsedNamespaces = usedNamespaceDeclarations
    member _.Visibility with get() = Model.VisibilityFlags.Unspecified
    member _.Flags with get() = flags and set value = flags <- value
    member _.InheritedTypes with get() = inheritedTypeDefinitions and set value = inheritedTypeDefinitions <- value
    member _.Methods with get() = methods and set value = methods <- value
    member _.Fields with get() = fields and set value = fields <- value

    override this.Equals o =
        match o with
        | :? CheckedTypeDefinition as other -> (this :> IEquatable<_>).Equals other
        | _ -> false

    override _.GetHashCode() = identifier.GetHashCode()

    interface IEquatable<CheckedTypeDefinition> with member _.Equals other = identifier = other.Identifier

type SemanticErrorMessage =
    | AmbiguousTypeIdentifier of TypeIdentifier * matches: seq<FullTypeIdentifier>
    | ArrayConstructorCall
    | DuplicateLocalDeclaration of ParsedIdentifier
    | DuplicateParameter of ParsedIdentifier
    | DuplicateTypeDefinition of FullTypeIdentifier
    | ExpectedExpressionType of expected: CheckedType * actual: CheckedType
    | InvalidCharacterType of ParsedNode<AnyTypeNode>
    | InvalidElementType of CheckedType
    | MultipleEntryPoints
    | TypeHasNoMethods of CheckedType
    | UndefinedLocal of ParsedIdentifier
    | UndefinedMethod of FullTypeIdentifier * methodName: IdentifierNode
    | UndefinedTypeIdentifier of TypeIdentifier
    | UnknownError of message: string

    override this.ToString() =
        match this with
        | AmbiguousTypeIdentifier(id, matches) ->
            System.Text.StringBuilder(id.ToString())
                .Append(" could refer to any of the following types: ")
                .AppendJoin(", ", matches)
                .ToString()
        | ArrayConstructorCall -> "Cannot create new array instance with a constructor call, use array literal syntax instead"
        | DuplicateLocalDeclaration id -> "Duplicate local declaration " + id.ToString()
        | DuplicateParameter id -> "A parameter with the name " + id.ToString() + " was already defined"
        | DuplicateTypeDefinition id -> "Duplicate definition of type " + id.ToString()
        | ExpectedExpressionType(expected, actual) ->
            "Expected an expression of type " + expected.ToString() + " but got " + actual.ToString()
        | InvalidCharacterType ty ->
            ty.Content.ToString() + " is not a valid character type"
        | InvalidElementType ty ->
            ty.ToString() + " is not a valid element type, only value types and reference types are allowed"
        | MultipleEntryPoints -> "Only one entry point method in a module is allowed"
        | TypeHasNoMethods ty -> ty.ToString() + " is a type that does not define any methods"
        | UndefinedLocal id -> "A local or parameter with the name " + id.ToString() + " does not exist"
        | UndefinedMethod(declaringTypeName, methodName) ->
            "A method named " + methodName.Content.ToString() + " could not be found in the type " + declaringTypeName.ToString()
        | UndefinedTypeIdentifier id -> "The type definition " + id.ToString() + " does not exist"
        | UnknownError message -> message

type SemanticError =
    { Line: uint32
      Column: uint32
      Source: ParsedFile
      Message: SemanticErrorMessage }

[<Sealed>]
type CheckedModule
    (
        name: Model.Name,
        version: Model.VersionNumbers,
        importedModules: ImmutableArray<UByte.Resolver.ResolvedModule>,
        definedTypes: ImmutableArray<CheckedTypeDefinition>,
        importedTypes: ImmutableArray<UByte.Resolver.ResolvedTypeDefinition>,
        definedFields: ImmutableArray<CheckedField>,
        definedMethods: ImmutableArray<CheckedMethod>,
        importedMethods: ImmutableArray<UByte.Resolver.ResolvedMethod>,
        entryPointMethod: CheckedMethod voption,
        errors: ImmutableArray<SemanticError>
    )
    =
    member val Identifier = { Model.ModuleIdentifier.ModuleName = name; Model.ModuleIdentifier.Version = version }
    member this.Name = this.Identifier.ModuleName
    member this.Version = this.Identifier.Version
    member _.ImportedModules = importedModules
    member _.ImportedTypes = importedTypes
    member _.DefinedTypes = definedTypes
    member _.DefinedFields = definedFields
    member _.ImportedMethods = importedMethods
    member _.DefinedMethods = definedMethods
    member _.EntryPoint = entryPointMethod
    member _.Errors = errors

[<RequireQualifiedAccess>]
module CheckedType =
    let primitives = Dictionary()

    let primitive ptype =
        match primitives.TryGetValue ptype with
        | true, existing -> existing
        | false, _ ->
            let ty = CheckedType.ValueType(CheckedValueType.Primitive ptype)
            primitives.[ptype] <- ty
            ty

    let arrays = Dictionary()

    let array etype = //  TODO: Don't have a global cache for array types.
        match arrays.TryGetValue etype with
        | true, existing -> existing
        | false, _ ->
            let ty = CheckedType.ReferenceType(CheckedReferenceType.Array etype)
            arrays.[etype] <- ty
            ty

[<RequireQualifiedAccess>]
module CheckedElementType =
    let primitives = Dictionary()

    let primitive ptype =
        match primitives.TryGetValue ptype with
        | true, existing -> existing
        | false, _ ->
            let ty = CheckedElementType.ValueType(CheckedValueType.Primitive ptype)
            primitives.[ptype] <- ty
            ty

[<RequireQualifiedAccess>]
module SemanticError =
    let ofNode (node: ParsedNode<_>) source message =
        { Line = node.Line; Column = node.Column; Source = source; Message = message }

[<RequireQualifiedAccess>]
module TypeChecker =
    let private addErrorMessage (errors: ImmutableArray<_>.Builder) node source message =
        errors.Add(SemanticError.ofNode node source message)

    [<Struct>]
    type CheckResultBuilder =
        member inline _.Bind(result: Result<'T, SemanticError>, body: 'T -> _) =
            match result with
            | Ok value -> body value
            | Error error -> Error error

        member inline _.Return value = Result<'T, SemanticError>.Ok value

        member inline _.ReturnFrom(value: Result<'T, SemanticError>) = value

    let private validated = CheckResultBuilder()

    let private findTypeDeclarations errors (files: ImmutableArray<ParsedFile>) =
        /// Checks and adds all declarations inside of a file or namespace block.
        let rec inner
            errors
            file
            (lookup: Dictionary<_, _>)
            (currentNamespaceName: FullNamespaceName)
            (parentUsingDeclarations: ImmutableArray<_>)
            (nodes: ParsedNodeArray<TopLevelNode>)
            =
            let usings = ImmutableArray.CreateBuilder<FullNamespaceName>()
            let declaredTypeNodes = List<_ -> CheckedTypeDefinition>()
            let nestedNamespaceDeclarations = List()

            usings.Add currentNamespaceName
            usings.AddRange parentUsingDeclarations

            for node in nodes do
                match node.Content with
                | TopLevelNode.NamespaceDeclaration(ns, nested) ->
                    nestedNamespaceDeclarations.Add(struct(ns, nested))
                | TopLevelNode.TypeDeclaration(name, attributes, extends, members) ->
                    declaredTypeNodes.Add <| fun usedNamespaceDeclarations ->
                        CheckedTypeDefinition (
                            FullTypeIdentifier.create name (FullNamespaceName.toParsedName currentNamespaceName),
                            file,
                            attributes,
                            extends,
                            members,
                            usedNamespaceDeclarations
                        )
                | TopLevelNode.UsingNamespace ns ->
                    usings.Add(FullNamespaceName ns)
                | TopLevelNode.Error msg -> // TODO: Error nodes should be treated as UserState in parser stage, TypeChecker should have to deal with parser errors.
                    addErrorMessage errors node file (UnknownError msg)

            let currentUsingDeclarations = usings.ToImmutable()

            for declaredTypeBuilder in declaredTypeNodes do
                let ty = declaredTypeBuilder currentUsingDeclarations
                if not(lookup.TryAdd(ty.Identifier, ty)) then
                    addErrorMessage errors ty.Identifier.Name ty.Source (DuplicateTypeDefinition ty.Identifier)

            for struct(nestedNamespaceName, nested) in nestedNamespaceDeclarations do
                inner errors file lookup (FullNamespaceName.append nestedNamespaceName currentNamespaceName) currentUsingDeclarations nested

        let lookup = Dictionary<FullTypeIdentifier, CheckedTypeDefinition> EqualityComparer.Default
        for file in files do inner errors file lookup FullNamespaceName.empty ImmutableArray.Empty file.Nodes
        lookup

    let private importedTypeLookup (imports: ImmutableArray<UByte.Resolver.ResolvedModule>.Builder) =
        let lookup = Dictionary<FullTypeIdentifier, UByte.Resolver.ResolvedTypeDefinition voption>()
        let resolved = ImmutableArray.CreateBuilder()
        let search id =
            match lookup.TryGetValue id with
            | true, existing -> existing
            | false, _ ->
                let ns = FullNamespaceName.toStrings id.Namespace
                let mutable i, ty = 0, ValueNone

                while i < imports.Count && ty.IsNone do
                    let md = imports.[i]
                    ty <- md.TryFindType(ns, id.Name.Content.ToString())
                    i <- i + 1

                if ty.IsSome then
                    resolved.Add ty.Value

                lookup.Add(id, ty)
                ty
        struct(resolved, search)

    let private matchingNamedTypes namedTypeLookup (namespaces: ImmutableArray<FullNamespaceName>) (id: TypeIdentifier) =
        let matches = List<FullTypeIdentifier * NamedType>()

        let search ns =
            let identifier = FullTypeIdentifier.ofTypeIdentifier ns &id
            match namedTypeLookup identifier with
            | ValueSome ty -> matches.Add((identifier, ty))
            | ValueNone -> ()

        search FullNamespaceName.empty
        for ns in namespaces do search ns
        matches

    /// <summary>
    /// Searches for a type corresponding to the <paramref name="identifier"/> given the <paramref name="namespaces"/> that are
    /// currently in scope.
    /// </summary>
    /// <param name="namedTypeLookup">Function used to retrieve a type definition given its exact namespace and name.</param>
    /// <param name="namespaces">The list of namespaces that are currently in scope.</param>
    /// <param name="identifier">The name of the type.</param>
    let private findNamedType namedTypeLookup namespaces identifier =
        let matches = matchingNamedTypes namedTypeLookup namespaces identifier
        match matches.Count with
        | 1 -> Ok matches.[0]
        | 0 -> Error(UndefinedTypeIdentifier identifier)
        | _ -> Error(AmbiguousTypeIdentifier(identifier, Seq.map fst matches))

    let private checkAnyType namedTypeLookup: _ -> Result<_, SemanticError> =
        let cachedTypeLookup valueFactory =
            let lookup = Dictionary<'Key, 'Value>()
            fun key ->
                match lookup.TryGetValue key with
                | true, existing -> Ok existing
                | false, _ ->
                    match valueFactory key with
                    | Ok ty as result ->
                        lookup.Add(key, ty)
                        result
                    | Error _ as error -> error

        let primitiveTypeCache = cachedTypeLookup (CheckedValueType.Primitive >> CheckedType.ValueType >> Ok)
        let valueElementTypes = cachedTypeLookup (CheckedElementType.ValueType >> Ok)
        let referenceElementTypes = cachedTypeLookup (CheckedElementType.ReferenceType >> Ok)
        let arrayTypeCache = cachedTypeLookup <| fun etype -> //(CheckedReferenceType.Array >> CheckedType.ReferenceType >> Ok)
            match etype with
            | CheckedType.ValueType vt -> valueElementTypes vt
            | CheckedType.ReferenceType rt -> referenceElementTypes rt
            | CheckedType.Void -> invalidOp "Cannot use void as the element type of an array"
            |> Result.map (CheckedReferenceType.Array >> CheckedType.ReferenceType)

        let rec inner (atype: ParsedNode<AnyTypeNode>) =
            match atype.Content with
            | AnyTypeNode.Primitive prim -> primitiveTypeCache prim
            | AnyTypeNode.Array elemt ->
                match inner elemt with
                | Ok etype -> arrayTypeCache etype
                | Error _ as error -> error
        inner

    let private checkTypeAttributes errors (declarations: Dictionary<FullTypeIdentifier, _>) namedTypeLookup =
        let inheritedTypeBuilder = ImmutableArray.CreateBuilder<NamedType>()

        for (ty: CheckedTypeDefinition) in declarations.Values do
            let mutable flags = Model.TypeDefinitionFlags.Final

            for attr in ty.AttributeNodes do
                match attr.Content with
                //| TypeAttributeNode.Abstract when flags.IsSet Model.TypeDefinitionFlags.NotFinal
                //| TypeAttributeNode.Instance when flags.IsSet Model.TypeDefinitionFlags.Abstract ->
                //    addErrorMessage
                | TypeAttributeNode.Abstract ->
                    flags <- flags ||| Model.TypeDefinitionFlags.Abstract ||| Model.TypeDefinitionFlags.NotFinal
                | TypeAttributeNode.Inheritable ->
                    flags <- flags ||| Model.TypeDefinitionFlags.NotFinal
                // TODO: Check for duplicate type attributes

            inheritedTypeBuilder.Clear()

            for inheritedTypeName in ty.InheritanceNodes do
                match findNamedType namedTypeLookup ty.UsedNamespaces inheritedTypeName.Content with
                | Ok(_, ty) -> inheritedTypeBuilder.Add ty
                | Error error -> addErrorMessage errors inheritedTypeName ty.Source error

            ty.Flags <- flags
            ty.InheritedTypes <- inheritedTypeBuilder.ToImmutable()

    let private findTypeMembers errors (declarations: Dictionary<FullTypeIdentifier, _>) =
        let methodNameLookup = Dictionary<CheckedTypeDefinition, _>()
        let allDefinedMethods = ImmutableArray.CreateBuilder()
        /// Reused instance used to build an array containing all of a type definition's methods.
        let definedTypeMethods = ImmutableArray.CreateBuilder()

        let fieldNameLookup = Dictionary<CheckedTypeDefinition, _>()
        let allDefinedFields = ImmutableArray.CreateBuilder()
        let definedTypeFields = ImmutableArray.CreateBuilder()

        for ty in declarations.Values do
            let methods = Dictionary<ParsedIdentifier, CheckedMethod>() // TODO: Allow overloading by parameter count AND parameter types.
            let fields = Dictionary<ParsedIdentifier, CheckedField>()

            methodNameLookup.Add(ty, methods)
            fieldNameLookup.Add(ty, fields)

            for node in ty.MemberNodes do
                match node.Content with
                | TypeMemberNode.MethodDeclaration(name, attributes, parameters, returns, body) ->
                    // TODO: Check for duplicate methods.
                    let method =
                        CheckedMethod (
                            ty,
                            name,
                            attributes,
                            parameters,
                            returns,
                            body
                        )

                    methods.Add(name.Content, method)
                    definedTypeMethods.Add method
                    allDefinedMethods.Add method
                | TypeMemberNode.FieldDeclaration(name, attributes, fieldType, initialValue) ->
                    if initialValue.IsSome then raise(NotImplementedException "Fields with initial values are not yet supported")
                    let field = CheckedField(ty, name, attributes)
                    fields.Add(name.Content, field)
                    definedTypeFields.Add field
                    allDefinedFields.Add field

            ty.Methods <- definedTypeMethods.ToImmutable()
            ty.Fields <- definedTypeFields.ToImmutable()

            definedTypeMethods.Clear()
            definedTypeFields.Clear()

        struct(allDefinedFields, fieldNameLookup, allDefinedMethods, methodNameLookup)

    let private checkDefinedMethods
        (errors: ImmutableArray<_>.Builder)
        anyTypeChecker
        (declarations: Dictionary<CheckedTypeDefinition, Dictionary<ParsedIdentifier, CheckedMethod>>)
        (entryPointMethod: byref<_ voption>)
        =
        let parameterNameLookup = HashSet<ParsedIdentifier>()
        let parameters = ImmutableArray.CreateBuilder()
        let returns = ImmutableArray.CreateBuilder()

        for definedMethods in declarations.Values do
            for method in definedMethods.Values do
                for attr in method.AttributeNodes do
                    match attr.Content with
                    | MethodAttributeNode.Entrypoint ->
                        match entryPointMethod with
                        | ValueNone -> entryPointMethod <- ValueSome method
                        | ValueSome _ -> addErrorMessage errors attr method.DeclaringType.Source MultipleEntryPoints
                    | MethodAttributeNode.Private ->
                        // TODO: Check for duplicate visibility flags
                        method.Visibility <- Model.VisibilityFlags.Private
                    | MethodAttributeNode.Public ->
                        // TODO: Check for duplicate visibility flags
                        method.Visibility <- Model.VisibilityFlags.Public
                    | _ ->
                        failwith "TODO: Set other method flags"

                parameterNameLookup.Clear()

                for (name, ty) in method.ParameterNodes do
                    match anyTypeChecker ty with
                    | Ok ptype ->
                        if parameterNameLookup.Add name.Content then
                            parameters.Add { CheckedParameter.Name = name; CheckedParameter.Type = ptype }
                        else
                            addErrorMessage errors name method.DeclaringType.Source (DuplicateParameter name.Content)
                    | Error error ->
                        errors.Add error
                    // TODO: If an error occurs, add a placeholder parameter

                returns.Clear()

                for ty in method.ReturnTypeNodes do
                    match anyTypeChecker ty with
                    | Ok rtype -> returns.Add rtype
                    | Error error -> errors.Add error
                    // TODO: If an error occurs, add a placeholder return type

                method.Parameters <- parameters.ToImmutable()
                method.ReturnTypes <- returns.ToImmutable()

    /// Compares local variables, ensuring that a local variable that is nested does not share a name with a local variable in an
    /// outer scope.
    let private localVariableComparer = {
        new IEqualityComparer<CheckedLocal> with
            member _.GetHashCode local = hash local.Name.Content
            member _.Equals(x, y) =
                if x.Name.Content = y.Name.Content then
                    failwith "TODO: Compare them"
                else false
    }

    let private voidReturnValues = ImmutableArray.Create<CheckedType> CheckedType.Void

    /// Contains helper functions for translating type signatures from imported modules
    [<RequireQualifiedAccess>]
    module TranslateType =
        open UByte.Format.Model

        let valueType (im: UByte.Resolver.ResolvedModule) vt =
            match vt with
            | ValueType.Primitive pt -> CheckedValueType.Primitive pt
            | _ -> raise(NotImplementedException(sprintf "Translation of value type %O is not yet supported" vt))

        let rec referenceType (im: UByte.Resolver.ResolvedModule) rt =
            match rt with
            | ReferenceType.Vector etype -> CheckedReferenceType.Array(elementType im etype)
            | _ -> raise(NotImplementedException(sprintf "Translation of reference type %O is not yet supported" rt))

        and elementType im et =
            match et with
            | ReferenceOrValueType.Value vt -> CheckedElementType.ValueType(valueType im vt)
            | ReferenceOrValueType.Reference rt -> CheckedElementType.ReferenceType(referenceType im rt)

        let rec signature im t =
            match t with
            | AnyType.ValueType vt -> CheckedType.ValueType(valueType im vt)
            | AnyType.ReferenceType rt -> CheckedType.ReferenceType(referenceType im rt)
            | _ -> raise(NotImplementedException(sprintf "Translation of type signature %O is not yet supported" t))

        let methodImportTypes (im: UByte.Resolver.ResolvedModule) (types: ImmutableArray<_>) =
            if not types.IsDefaultOrEmpty then
                let mutable translated = Array.zeroCreate types.Length
                for i = 0 to translated.Length - 1 do
                    translated.[i] <- signature im (im.TypeSignatureAt types.[i])
                Unsafe.As<CheckedType[], ImmutableArray<CheckedType>> &translated
            else
                voidReturnValues

    let private checkMethodBodies
        (errors: ImmutableArray<_>.Builder)
        namedTypeLookup
        (namedMethodLookup: _ -> _ -> NamedMethod voption)
        (methodImportLookup: HashSet<_>)
        (declarations: Dictionary<CheckedTypeDefinition, Dictionary<ParsedIdentifier, CheckedMethod>>)
        =
        let localVariableLookup = Dictionary<ParsedIdentifier, CheckedType>() // HashSet<CheckedLocal> localVariableComparer
        (*
        // TODO: This should be valid middlec code:
        if (some condition) {
            let a bool = true;
        }
        else {
           let a u32 = 5u;
        }
        *)
        //let inScopeLocals = Stack()

        let rec checkParsedExpression (method: CheckedMethod) expr: Result<_, SemanticError> =
            let inline ok expression ty =
                Ok { TypedExpression.Expression = expression; Type = ty; Node = expr }

            let inline error node message =
                Error(SemanticError.ofNode node method.DeclaringType.Source message)

            match expr.Content with
            | ExpressionNode.LiteralBool value ->
                ok (CheckedExpression.LiteralBoolean value) (CheckedType.primitive Model.PrimitiveType.Bool)
            | ExpressionNode.LiteralS32 value ->
                ok (CheckedExpression.LiteralSignedInteger(int64 value)) (CheckedType.primitive Model.PrimitiveType.S32)
            | ExpressionNode.NewObject({ Content = AnyTypeNode.Array etype } as ty, oconstructor) ->
                match oconstructor.Content with
                | ConstructionExpression.ConstructorCall _ ->
                    error oconstructor SemanticErrorMessage.ArrayConstructorCall
                | ConstructionExpression.String str ->
                    match etype.Content with
                    | AnyTypeNode.Primitive Model.PrimitiveType.Char32 ->
                        let characters = ImmutableArray.CreateBuilder()

                        for rune in str.EnumerateRunes() do
                            { TypedExpression.Expression = CheckedExpression.LiteralUnsignedInteger(uint64 rune.Value)
                              TypedExpression.Type = CheckedType.primitive Model.PrimitiveType.Char32
                              TypedExpression.Node = expr }
                            |> characters.Add

                        let etype = CheckedElementType.primitive Model.PrimitiveType.Char32
                        ok (CheckedExpression.NewArray(etype, characters.ToImmutable())) (CheckedType.array etype)
                    // TODO: Add case for UTF-16 strings
                    | _ -> error ty (SemanticErrorMessage.InvalidCharacterType etype)
                | bad -> raise(NotImplementedException "TODO: Add support for array literals")
            // TODO: When nested types are supported, use :: to refer to nested types.
            | ExpressionNode.MemberAccess(source, name, ValueSome arguments) ->
                validated {
                    let! src = checkSourceExpression method source
                    let! declaringTypeName, declaringTypeDefinition = determineSourceType method expr src

                    match namedMethodLookup declaringTypeDefinition name.Content with
                    | ValueSome callee ->
                        let! methodArgumentExpressions = checkArgumentExpressions method arguments
                        let methodReturnTypes =
                            let inline multipleReturnTypesNotSupported() =
                                raise(NotImplementedException "Methods with multiple return types are not yet supported")
                            match callee with
                            | Choice1Of2 defined ->
                                match defined.ReturnTypes.Length with
                                | 0 -> voidReturnValues
                                | 1 -> defined.ReturnTypes
                                | _ -> multipleReturnTypesNotSupported()
                            | Choice2Of2 imported ->
                                methodImportLookup.Add imported |> ignore
                                let rtypes = imported.Signature.ReturnTypes
                                match rtypes.Length with
                                | 0 -> voidReturnValues
                                | 1 -> TranslateType.methodImportTypes imported.DeclaringModule rtypes
                                | _ -> multipleReturnTypesNotSupported()

                        match src with
                        | Choice1Of2 _ ->
                            return! ok (CheckedExpression.MethodCall(callee, methodArgumentExpressions)) methodReturnTypes.[0]
                        | Choice2Of2 _ ->
                            return! raise(NotImplementedException "Add support for instance methods e.g. myLocal.MyMethod()")
                    | ValueNone ->
                        return! error name (SemanticErrorMessage.UndefinedMethod(declaringTypeName, name))
                }
            | ExpressionNode.MemberAccess(source, name, ValueNone) ->
                validated {
                    let! src = checkSourceExpression method source
                    match src, name.Content.ToString() with
                    | Choice2Of2({ Type = CheckedType.ReferenceType(CheckedReferenceType.Array _) } as array), "length" ->
                        return! ok (CheckedExpression.ArrayLengthAccess array) (CheckedType.primitive Model.PrimitiveType.UNative)
                    | _, _ ->
                        let! declaringTypeName, declaringTypeDefinition = determineSourceType method expr src
                        return raise(NotImplementedException "TODO: Field access")
                }
            | ExpressionNode.Local name ->
                match localVariableLookup.TryGetValue name.Content with
                | true, ltype -> ok (CheckedExpression.Local name.Content) ltype
                | false, _ -> error name (SemanticErrorMessage.UndefinedLocal name.Content)
            | ExpressionNode.BinaryOperation(op, x, y) ->
                validated {
                    let! xexpr = checkParsedExpression method x
                    let! yexpr = checkParsedExpression method y
                    // TODO: Have support for implicit casting, this means that a helper function needs to decide whether y is a subtype of/compatible with x.
                    // TODO: Prohibit implicit narrowing (long to int is bad)
                    // TODO: Check that the type of arguments are correct, some shift operators may expect a certain integer type for an argument.
                    return!
                        match op with
                        | BinaryOperation.LessThan | BinaryOperation.LessThanOrEqual
                        | BinaryOperation.GreaterThan | BinaryOperation.GreaterThanOrEqual
                        | BinaryOperation.IsEqual | BinaryOperation.IsNotEqual ->
                            CheckedType.primitive Model.PrimitiveType.Bool
                        | _ -> xexpr.Type // TODO: Use largest type of two operands.
                        |> ok (CheckedExpression.BinaryOperation(op, xexpr, yexpr))
                }
            | bad -> raise(NotImplementedException(sprintf "TODO: Add support for expression %O" bad))

        and checkSourceExpression method (source: Choice<ParsedNamespaceName, _>) =
            match source with
            | Choice1Of2 named ->
                if named.Length = 0 then
                    invalidOp "Cannot determine if source is a namespace or local variable since node was unexpectedly empty, parser possibly interpreted local variable usage as a namespace or type name"

                let mutable localVariableType = Unchecked.defaultof<_>

                if named.Length = 1 && localVariableLookup.TryGetValue(named.[0].Content, &localVariableType) then
                    let localVariableName = named.[0]
                    Choice2Of2
                        { TypedExpression.Expression = CheckedExpression.Local localVariableName.Content
                          TypedExpression.Type = localVariableType
                          TypedExpression.Node =
                            { ParsedNode.Content = ExpressionNode.Local localVariableName
                              Line = localVariableName.Line
                              Column = localVariableName.Column } }
                else
                    Choice1Of2 named
                |> Ok
            | Choice2Of2 expression ->
                Result.map Choice2Of2 (checkParsedExpression method expression)

        and determineSourceType (method: CheckedMethod) errorNode (source: Choice<ParsedNamespaceName, TypedExpression>) =
            match source with
            | Choice1Of2 named ->
                let typeNameIndex = named.Length - 1

                findNamedType
                    namedTypeLookup
                    method.DeclaringType.UsedNamespaces
                    { TypeIdentifier.Name = named.[typeNameIndex]
                      TypeIdentifier.Namespace = named.RemoveAt(named.Length - 1) }
                |> Result.mapError (SemanticError.ofNode errorNode method.DeclaringType.Source)
            | Choice2Of2 inner ->
                match inner.Type with
                //| CheckedType.ReferenceType(CheckedReferenceType.Defined)
                | bad ->
                    Error(SemanticError.ofNode inner.Node method.DeclaringType.Source (SemanticErrorMessage.TypeHasNoMethods bad))

        and checkArgumentExpressions method (arguments: ImmutableArray<_>) =
            if arguments.IsDefaultOrEmpty then
                Ok ImmutableArray.Empty
            else
                let mutable expressions, i, error = Array.zeroCreate arguments.Length, 0, ValueNone

                while i < expressions.Length && error.IsNone do
                    match checkParsedExpression method arguments.[i] with
                    | Ok expr -> expressions.[i] <- expr
                    | Error err -> error <- ValueSome err
                    i <- i + 1

                match error with
                | ValueNone -> Ok(Unsafe.As<TypedExpression[], ImmutableArray<TypedExpression>> &expressions)
                | ValueSome err -> Error err

        let checkConditionExpression method expression = validated {
            let tbool = CheckedType.primitive Model.PrimitiveType.Bool
            let! condition = checkParsedExpression method expression
            if condition.Type.Equals tbool then
                return condition
            else
                return!
                    SemanticErrorMessage.ExpectedExpressionType(tbool, condition.Type)
                    |> SemanticError.ofNode expression method.DeclaringType.Source
                    |> Error
        }

        /// Helper function to check a nested block, keeps a cache of ImmutableArray.Builder instances to build the statements.
        let mutable checkNestedStatements = Unchecked.defaultof<_>

        let checkBlockNodes method (statements: ImmutableArray<_>.Builder) (*localVariableLookup*) (nodes: ImmutableArray<_>) =
            let mutable success = true

            for bodyStatementNode in nodes do
                let result =
                    match bodyStatementNode.Content with
                    | StatementNode.Expression expression ->
                        Result.map
                            CheckedStatement.Expression
                            (checkParsedExpression method expression)
                    | StatementNode.If(condition, trueStatementNodes, additionalStatementNodes, falseStatementNodes) ->
                        if not additionalStatementNodes.IsDefaultOrEmpty then
                            raise(NotImplementedException "TODO: elif is not yet supported by type checker")

                        validated {
                            let! cond = checkConditionExpression method condition
                            let thenBlockStatements = checkNestedStatements method trueStatementNodes
                            let elseBlockStatements = checkNestedStatements method falseStatementNodes
                            return CheckedStatement.If(cond, thenBlockStatements, elseBlockStatements)
                        }
                    | StatementNode.LocalDeclaration(constant, name, ty, value) ->
                        let lname = name.Content
                        if localVariableLookup.ContainsKey lname then
                            Error(SemanticError.ofNode name method.DeclaringType.Source (DuplicateLocalDeclaration lname))
                        else
                            localVariableLookup.[lname] <- CheckedType.Void
                            validated {
                                // TODO: Check that type of local and type of value is compatible.
                                let! ltype = checkAnyType namedTypeLookup ty
                                localVariableLookup.[lname] <- ltype
                                let! lvalue = checkParsedExpression method value
                                return CheckedStatement.LocalDeclaration(constant, name, ltype, lvalue)
                            }
                    | StatementNode.Return value(*s*) ->
                        // TODO: Check that types of return values match method return types
                        validated {
                            let! expr = checkParsedExpression method value
                            return CheckedStatement.Return(ImmutableArray.Create expr)
                        }
                    | StatementNode.While(condition, body) ->
                        validated {
                            let! cond = checkConditionExpression method condition
                            let statements = checkNestedStatements method body
                            return CheckedStatement.While(cond, statements)
                        }
                    | StatementNode.Empty -> Ok CheckedStatement.Empty
                    | bad -> raise(NotImplementedException(sprintf "TODO: Add support for statement %O" bad))

                match result with
                | Ok statement ->
                    statements.Add statement
                | Error error ->
                    success <- false
                    errors.Add error

            // TODO: Detect if code already defines a return, and omit the following insertion/error handler.
            if method.ReturnTypes.IsDefaultOrEmpty then
                statements.Add(CheckedStatement.Return ImmutableArray.Empty)
            //else
            //    failwith "TODO: Error for missing return"

            statements.ToImmutable()

        checkNestedStatements <-
            let nestedBlockStatements = Stack<ImmutableArray<CheckedStatement>.Builder>()
            fun method nodes ->
                let builder =
                    match nestedBlockStatements.TryPop() with
                    | true, statements ->
                        statements.Clear()
                        statements
                    | false, _ ->
                        ImmutableArray.CreateBuilder()

                let result = checkBlockNodes method builder nodes
                nestedBlockStatements.Push builder
                result

        let statements = ImmutableArray.CreateBuilder<CheckedStatement>()

        for methods in declarations.Values do
            for method in methods.Values do
                statements.Clear()
                localVariableLookup.Clear()

                for parameter in method.Parameters do
                    let name = parameter.Name.Content
                    if not(localVariableLookup.TryAdd(name, parameter.Type)) then
                        SemanticError.ofNode
                            parameter.Name
                            method.DeclaringType.Source
                            (DuplicateLocalDeclaration name)
                        |> errors.Add

                method.Body <-
                    match method.BodyNode with
                    | MethodBodyNode.Defined nodes ->
                        //match checkBlockNodes method statements nodes with
                        //| ValueSome body ->
                            CheckedMethodBody.Defined(checkBlockNodes method statements nodes)
                        //| ValueNone ->
                        //    CheckedMethodBody.Error
                    | MethodBodyNode.External(name, library) ->
                        CheckedMethodBody.External(name.Content, library.Content)

    let check name version files (imports: ImmutableArray<_>) =
        let errors = ImmutableArray.CreateBuilder()

        let moduleImportList = ImmutableArray.CreateBuilder<UByte.Resolver.ResolvedModule>()
        moduleImportList.AddRange imports

        let declaredTypesLookup = findTypeDeclarations errors files

        // TOOD: Add to module import list if any other resolved types are used.
        let struct(typeImportList, typeImportLookup) = importedTypeLookup moduleImportList

        let namedTypeLookup: _ -> NamedType voption =
            fun id ->
                match declaredTypesLookup.TryGetValue id with
                | true, declared -> ValueSome(Choice1Of2 declared)
                | false, _ ->
                    match typeImportLookup id with
                    | ValueSome imported -> ValueSome(Choice2Of2 imported)
                    | ValueNone -> ValueNone

        checkTypeAttributes errors declaredTypesLookup namedTypeLookup

        let anyTypeChecker = checkAnyType namedTypeLookup

        let struct(allDefinedFields, fieldNameLookup, allDefinedMethods, methodNameLookup) =
            findTypeMembers errors declaredTypesLookup

        let namedMethodLookup: NamedType -> ParsedIdentifier -> NamedMethod voption =
            fun declaringType methodName ->
                match declaringType with
                | Choice1Of2 defined ->
                    match methodNameLookup.[defined].TryGetValue methodName with
                    | true, method -> ValueSome(Choice1Of2 method)
                    | false, _ -> ValueNone
                | Choice2Of2 imported ->
                    // TODO: Figure out how to allow overloads of imported methods in middlec.
                    ValueOption.map Choice2Of2 (imported.TryFindMethod(methodName.ToString()))

        let mutable entryPointMethod = ValueNone
        checkDefinedMethods errors anyTypeChecker methodNameLookup &entryPointMethod

        let methodImportLookup = HashSet()

        // The types defined in this module, types of fields, and types of methods have been determined, so analysis of method
        // bodies can begin.
        checkMethodBodies errors namedTypeLookup namedMethodLookup methodImportLookup methodNameLookup

        CheckedModule (
            name = Model.Name.ofStr name,
            version = Model.VersionNumbers(ImmutableArray.CreateRange version),
            importedModules = moduleImportList.ToImmutable(),
            definedTypes = declaredTypesLookup.Values.ToImmutableArray(),
            definedFields = allDefinedFields.ToImmutable(),
            definedMethods = allDefinedMethods.ToImmutable(),
            importedTypes = typeImportList.ToImmutable(),
            importedMethods = methodImportLookup.ToImmutableArray(),
            entryPointMethod = entryPointMethod,
            errors =
                if errors.Count <> errors.Capacity
                then errors.ToImmutable()
                else errors.MoveToImmutable()
        )
