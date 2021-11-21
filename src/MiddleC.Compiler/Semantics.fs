namespace MiddleC.Compiler.Semantics

open System
open System.Collections.Generic
open System.Collections.Immutable

open MiddleC.Compiler.Parser

open UByte.Format

[<Struct; NoComparison; StructuralEquality>]
type FullNamespaceName =
    internal
    | FullNamespaceName of ParsedNamespaceName

[<RequireQualifiedAccess>]
module FullNamespaceName =
    let empty = FullNamespaceName ImmutableArray.Empty

    let append (nested: ParsedNamespaceName) (FullNamespaceName parent) =
        FullNamespaceName(parent.AddRange nested)

    let toParsedName (FullNamespaceName ns) = ns

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

type SemanticErrorMessage =
    | AmbiguousTypeIdentifier of TypeIdentifier * matches: seq<FullTypeIdentifier>
    | DuplicateParameter of ParsedIdentifier
    | DuplicateTypeDefinition of FullTypeIdentifier
    | MultipleEntryPoints
    | UndefinedTypeIdentifier of TypeIdentifier
    | UnknownError of message: string

    override this.ToString() =
        match this with
        | AmbiguousTypeIdentifier(id, matches) ->
            System.Text.StringBuilder(id.ToString())
                .Append(" could refer to any of the following types: ")
                .AppendJoin(", ", matches)
                .ToString()
        | DuplicateParameter id -> "A parameter with the name " + id.ToString() + " was already defined"
        | DuplicateTypeDefinition id -> "Duplicate definition of type " + id.ToString()
        | MultipleEntryPoints -> "Only one entry point method in a module is allowed"
        | UndefinedTypeIdentifier id -> "The type definition " + id.ToString() + " does not exist"
        | UnknownError message -> message

type SemanticError =
    { Line: uint32
      Column: uint32
      Source: ParsedFile
      Message: SemanticErrorMessage }

type NamedType = Choice<CheckedTypeDefinition, UByte.Resolver.ResolvedTypeDefinition>

and [<NoComparison; StructuralEquality>] CheckedValueType =
    | Primitive of Model.PrimitiveType

and [<NoComparison; StructuralEquality>] CheckedType =
    | ValueType of CheckedValueType

and [<RequireQualifiedAccess>] CheckedExpression =
    | LiteralBoolean of bool
    | LiteralSignedInteger of int64

    override this.ToString() =
        match this with
        | LiteralBoolean true -> "true"
        | LiteralBoolean false -> "false"
        | LiteralSignedInteger i -> string i

and TypedExpression =
    { Expression: CheckedExpression
      Type: CheckedType
      Node: ParsedNode<ExpressionNode> }

    override this.ToString() =
        match this with
        | { Expression = CheckedExpression.LiteralBoolean _ as expr } ->
            expr.ToString() // add casting syntax?
        | { Expression = CheckedExpression.LiteralSignedInteger _ as expr } ->
            match this.Type with
            | CheckedType.ValueType(CheckedValueType.Primitive prim) ->
                expr.ToString() // + prefix
            //| _ ->
            //    "figure out casting syntax (" + expr.ToString() + ")"

and [<RequireQualifiedAccess>] CheckedStatement =
    | Return of ImmutableArray<TypedExpression>
    | Empty

    override this.ToString() =
        match this with
        | Return value when value.IsDefaultOrEmpty -> "return;"
        | Return values -> System.Text.StringBuilder("return ").AppendJoin(", ", values).Append(';').ToString()
        | Empty -> ";"

and [<RequireQualifiedAccess>] CheckedParameter =
    { Name: IdentifierNode
      Type: CheckedType }

and [<RequireQualifiedAccess>] CheckedMethodBody =
    | Defined of ImmutableArray<CheckedStatement>

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
    member _.Visibility with get() = Model.VisibilityFlags.Unspecified
    member _.Flags with get() = flags and set value = flags <- value

    member _.Parameters
        with get() = parameters
        and set value =
            parameters <- value
            parameterTypes <- ImmutableArray.CreateRange(Seq.map (fun { CheckedParameter.Type = ty } -> ty) parameters)

    member _.ReturnTypes with get() = returns and set value = returns <- value
    member _.Signature = { CheckedMethodSignature.ReturnTypes = returns; CheckedMethodSignature.ParameterTypes = parameterTypes }
    member _.Body with get() = body and set value = body <- value

and NamedMethod = Choice<CheckedMethod, UByte.Resolver.ResolvedMethod>

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

    member _.Identifier = identifier
    member _.Source = source
    member _.AttributeNodes = typeAttributeNodes
    member _.InheritanceNodes = inheritedTypeNodes
    member _.MemberNodes = typeMemberNodes
    member _.UsedNamespaces = usedNamespaceDeclarations
    member _.Visibility with get() = Model.VisibilityFlags.Unspecified
    member _.Flags with get() = flags and set value = flags <- value
    member _.InheritedTypes with get() = inheritedTypeDefinitions and set value = inheritedTypeDefinitions <- value
    member _.Methods with get() = methods and set value = methods <- value

    override this.Equals o =
        match o with
        | :? CheckedTypeDefinition as other -> (this :> IEquatable<_>).Equals other
        | _ -> false

    override _.GetHashCode() = identifier.GetHashCode()

    interface IEquatable<CheckedTypeDefinition> with member _.Equals other = identifier = other.Identifier

[<Sealed>]
type CheckedModule
    (
        name: Model.Name,
        version: Model.VersionNumbers,
        imports: ImmutableArray<UByte.Resolver.ResolvedModule>,
        types: ImmutableArray<CheckedTypeDefinition>,
        main: CheckedMethod voption,
        errors: ImmutableArray<SemanticError>
    )
    =
    member val Identifier = { Model.ModuleIdentifier.ModuleName = name; Model.ModuleIdentifier.Version = version }
    member _.Name = name
    member _.Version = version
    member _.ImportedModules = imports
    member _.DefinedTypes = types
    member _.EntryPoint = main
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

[<RequireQualifiedAccess>]
module TypeChecker =
    let private addErrorMessage (errors: ImmutableArray<_>.Builder) (node: ParsedNode<_>) source message =
        errors.Add { Line = node.Line; Column = node.Column; Source = source; Message = message }

    let private findTypeDeclarations errors (files: ImmutableArray<ParsedFile>) =
        let rec inner
            errors
            file
            (lookup: Dictionary<_, _>)
            (currentNamespaceName: FullNamespaceName)
            (parentUsingDeclarations: ImmutableArray<_>)
            (nodes: ParsedNodeArray<TopLevelNode>)
            =
            let usings = ImmutableArray.CreateBuilder<FullNamespaceName>()
            let declaredTypeNodes = List()
            let nestedNamespaceDeclarations = List()

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
                    usings.Add(FullNamespaceName.append ns currentNamespaceName)
                | TopLevelNode.Error msg -> // TODO: Error nodes should be treated as UserState in parser stage, TypeChecker should have to deal with parser errors.
                    addErrorMessage errors node file (UnknownError msg)

            let currentUsingDeclarations = usings.ToImmutable()

            for declaredTypeBuilder in declaredTypeNodes do
                let ty = declaredTypeBuilder currentUsingDeclarations
                if not(lookup.TryAdd(ty.Identifier, ty)) then
                    addErrorMessage errors ty.Identifier.Name ty.Source (DuplicateTypeDefinition ty.Identifier)

            for struct(nestedNamespaceName, nested) in nestedNamespaceDeclarations do
                inner errors file lookup (FullNamespaceName.append nestedNamespaceName currentNamespaceName) currentUsingDeclarations nested

        let lookup = Dictionary<FullTypeIdentifier, CheckedTypeDefinition>()
        for file in files do inner errors file lookup FullNamespaceName.empty ImmutableArray.Empty file.Nodes
        lookup

    let private namespaceSearchString (FullNamespaceName ns) =
        if not ns.IsDefaultOrEmpty then
            let sb = System.Text.StringBuilder()
            for i = 0 to ns.Length - 1 do
                sb.Append ns.[i] |> ignore
                if i > 0 then sb.Append '.' |> ignore
            sb.ToString()
        else
            String.Empty

    let private importedTypeLookup (imports: ImmutableArray<UByte.Resolver.ResolvedModule>) =
        let lookup = Dictionary<FullTypeIdentifier, UByte.Resolver.ResolvedTypeDefinition voption>()
        fun id ->
            match lookup.TryGetValue id with
            | true, existing -> existing
            | false, _ ->
                let ns = namespaceSearchString id.Namespace
                let mutable i, ty = 0, ValueNone
                while i < imports.Length && ty.IsNone do
                    let md = imports.[i]
                    ty <- md.TryFindType(ns, id.Name.Content.ToString())
                    i <- i + 1
                lookup.Add(id, ty)
                ty

    let private matchingNamedTypes namedTypeLookup (namespaces: ImmutableArray<FullNamespaceName>) (id: TypeIdentifier) =
        let matches = List<FullTypeIdentifier * NamedType>()

        let inline search ns =
            let identifier = FullTypeIdentifier.ofTypeIdentifier ns &id
            match namedTypeLookup identifier with
            | ValueSome ty -> matches.Add((identifier, ty))
            | ValueNone -> ()

        search FullNamespaceName.empty
        for ns in namespaces do search ns

        matches

    let private findNamedType namedTypeLookup namespaces identifier =
        let matches = matchingNamedTypes namedTypeLookup namespaces identifier
        match matches.Count with
        | 1 -> Ok(snd matches.[0])
        | 0 -> Error(UndefinedTypeIdentifier identifier)
        | _ -> Error(AmbiguousTypeIdentifier(identifier, Seq.map fst matches))

    let private checkAnyType namedTypeLookup: _ -> Result<_, SemanticErrorMessage> =
        let primitives = Dictionary<Model.PrimitiveType, CheckedType>()

        fun (atype: ParsedNode<AnyTypeNode>) ->
            match atype.Content with
            | AnyTypeNode.Primitive prim ->
                match primitives.TryGetValue prim with
                | true, existing -> Ok existing
                | false, _ ->
                    let ty = CheckedType.ValueType(CheckedValueType.Primitive prim)
                    primitives.Add(prim, ty)
                    Ok ty
            | bad ->
                raise(NotImplementedException(sprintf "TODO: Add support for type %A" bad))

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
                | Ok ty -> inheritedTypeBuilder.Add ty
                | Error error -> addErrorMessage errors inheritedTypeName ty.Source error

            ty.Flags <- flags
            ty.InheritedTypes <- inheritedTypeBuilder.ToImmutable()

    let private findTypeMembers errors (declarations: Dictionary<FullTypeIdentifier, _>) =
        let methodNameLookup = Dictionary<CheckedTypeDefinition, _>()
        let definedMethodList = ImmutableArray.CreateBuilder()

        for ty in declarations.Values do
            let methods = Dictionary<ParsedIdentifier, CheckedMethod>() // TODO: Allow overloading by parameter count AND parameter types.
            methodNameLookup.Add(ty, methods)

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
                    definedMethodList.Add method
                //| TypeMemberNode.FieldDeclaration _

            ty.Methods <- definedMethodList.ToImmutable()

        struct((), methodNameLookup)

    let private checkDefinedMethods
        errors
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
                        addErrorMessage errors ty method.DeclaringType.Source error
                    // TODO: If an error occurs, add a placeholder parameter

                returns.Clear()

                for ty in method.ReturnTypeNodes do
                    match anyTypeChecker ty with
                    | Ok rtype -> returns.Add rtype
                    | Error error -> addErrorMessage errors ty method.DeclaringType.Source error
                    // TODO: If an error occurs, add a placeholder return type

                method.Parameters <- parameters.ToImmutable()
                method.ReturnTypes <- returns.ToImmutable()

    let private checkMethodBodies
        errors
        (declarations: Dictionary<CheckedTypeDefinition, Dictionary<ParsedIdentifier, CheckedMethod>>)
        =
        let statements = ImmutableArray.CreateBuilder<CheckedStatement>()
        let localVariableLookup = HashSet()
        let inScopeLocals = Stack()

        let checkParsedExpression expr =
            let inline ok expression ty =
                Ok { TypedExpression.Expression = expression; Type = ty; Node = expr }

            match expr.Content with
            | ExpressionNode.LiteralBool value ->
                ok (CheckedExpression.LiteralBoolean value) (CheckedType.primitive Model.PrimitiveType.Bool)
            | ExpressionNode.LiteralS32 value ->
                ok (CheckedExpression.LiteralSignedInteger(int64 value)) (CheckedType.primitive Model.PrimitiveType.S32)
            | bad -> raise(NotImplementedException(sprintf "TODO: Add support for expression %A" bad))

        for methods in declarations.Values do
            for method in methods.Values do
                statements.Clear()
                match method.BodyNode with
                | MethodBodyNode.Defined nodes ->
                    for bodyStatementNode in nodes do
                        match bodyStatementNode.Content with
                        | StatementNode.Return value(*s*) ->
                            // TODO: Check that types of return values match method return types
                            match checkParsedExpression value with
                            | Ok expr -> statements.Add(CheckedStatement.Return(ImmutableArray.Create expr))
                            | Error error -> addErrorMessage errors value method.DeclaringType.Source error
                        | StatementNode.Empty -> statements.Add CheckedStatement.Empty
                        | bad -> raise(NotImplementedException(sprintf "TODO: Add support for statement %A" bad))

                    method.Body <- CheckedMethodBody.Defined(statements.ToImmutable())
                | MethodBodyNode.External(name, library) ->
                    raise(NotImplementedException "TODO: External methods are not yet supported")

    let check name version files imports =
        let errors = ImmutableArray.CreateBuilder()
        let declaredTypesLookup = findTypeDeclarations errors files

        let typeImportLookup = importedTypeLookup imports

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

        let struct((), methodNameLookup) = findTypeMembers errors declaredTypesLookup
        let mutable entryPointMethod = ValueNone

        checkDefinedMethods errors anyTypeChecker methodNameLookup &entryPointMethod

        // The types defined in this module, types of fields, and types of methods have been determined, so analysis of method
        // bodies can begin.
        checkMethodBodies errors methodNameLookup

        CheckedModule (
            name = Model.Name.ofStr name,
            version = Model.VersionNumbers(ImmutableArray.CreateRange version),
            imports = imports,
            types = declaredTypesLookup.Values.ToImmutableArray(),
            main = entryPointMethod,
            errors =
                if errors.Count <> errors.Capacity
                then errors.ToImmutable()
                else errors.MoveToImmutable()
        )
