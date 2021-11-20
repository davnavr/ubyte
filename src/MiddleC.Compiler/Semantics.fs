namespace MiddleC.Compiler.Semantics

open System
open System.Collections.Generic
open System.Collections.Immutable

open MiddleC.Compiler.Parser

[<RequireQualifiedAccess>]
type CheckedModule =
    { Errors: ImmutableArray<string> }

[<RequireQualifiedAccess>]
module TypeChecker =
    [<NoComparison; NoEquality>]
    type private CheckingType =
        { Identifier: TypeIdentifier
          Attributes: ParsedNodeArray<TypeAttributeNode>
          Extends: ParsedNodeArray<TypeIdentifier>
          Members: ParsedNodeArray<TypeMemberNode>
          UsedNamespaces: ImmutableArray<ParsedNamespaceName> }

    let private findTypeDeclarations errors (files: ImmutableArray<ParsedFile>) =
        let rec inner
            (errors: ImmutableArray<_>.Builder)
            (lookup: Dictionary<_, _>)
            (types: ImmutableArray<_>.Builder)
            currentNamespaceName
            (parentUsingDeclarations: ImmutableArray<_>)
            (nodes: ParsedNodeArray<TopLevelNode>)
            =
            let usings = ImmutableArray.CreateBuilder()
            let declaredTypeNodes = List()
            let nestedNamespaceDeclarations = List()

            usings.AddRange parentUsingDeclarations

            for node in nodes do
                match node.Content with
                | TopLevelNode.NamespaceDeclaration(ns, nested) ->
                    nestedNamespaceDeclarations.Add(struct(ns, nested))
                | TopLevelNode.TypeDeclaration(name, attributes, extends, members) ->
                    declaredTypeNodes.Add({ TypeIdentifier.Name = name; TypeIdentifier.Namespace = currentNamespaceName }, attributes, extends, members)
                | TopLevelNode.UsingNamespace ns ->
                    usings.Add ns
                | TopLevelNode.Error msg ->
                    errors.Add msg

            let currentUsingDeclarations = usings.ToImmutable()

            for (id, attributes, extends, members) in declaredTypeNodes do
                // TODO: Handle duplicate type definitions
                let ty =
                    { CheckingType.Identifier = id
                      Attributes = attributes
                      Extends = extends
                      Members = members
                      UsedNamespaces = currentUsingDeclarations }

                lookup.Add(id, ty)
                types.Add ty

            for struct(nestedNamespaceName, nested) in nestedNamespaceDeclarations do
                inner errors lookup types nestedNamespaceName currentUsingDeclarations nested

        let lookup = Dictionary<TypeIdentifier, CheckingType>()
        let types = ImmutableArray.CreateBuilder()
        for file in files do inner errors lookup types ImmutableArray.Empty ImmutableArray.Empty file.Nodes
        struct(types.ToImmutable(), lookup)

    let private namespaceSearchString (ns: ParsedNamespaceName) =
        if not ns.IsDefaultOrEmpty then
            let sb = System.Text.StringBuilder()
            for i = 0 to ns.Length - 1 do
                sb.Append ns.[i] |> ignore
                if i > 0 then sb.Append '.' |> ignore
            sb.ToString()
        else
            String.Empty

    let private importedTypeLookup (imports: ImmutableArray<UByte.Resolver.ResolvedModule>) =
        let lookup = Dictionary<TypeIdentifier, UByte.Resolver.ResolvedTypeDefinition voption>()
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

    let private typeAttributeChecker (types: ImmutableArray<CheckingType>) =
        let lookup = Dictionary<TypeIdentifier, UByte.Format.Model.TypeDefinitionFlags * SomeType list> types.Length
        ()

    let check files imports =
        let errors = ImmutableArray.CreateBuilder()
        let struct(typeDeclarationList, typeDeclarationLookup) = findTypeDeclarations errors files
        let typeImportLookup = importedTypeLookup imports

        failwith "BAD"

        { CheckedModule.Errors =
            if errors.Count <> errors.Capacity
            then errors.ToImmutable()
            else errors.MoveToImmutable() }
