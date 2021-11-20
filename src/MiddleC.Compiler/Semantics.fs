namespace MiddleC.Compiler.Semantics

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open MiddleC.Compiler.Parser

[<RequireQualifiedAccess>]
type CheckedModule =
    { Errors: ImmutableArray<unit> }

[<RequireQualifiedAccess>]
module TypeChecker =
    [<IsReadOnly; Struct; NoComparison; StructuralEquality>]
    type TypeIdentifier = { Name: ParsedIdentifier; Namespace: ParsedNamespaceName } // TODO: Move this to parser namespace.

    [<NoComparison; NoEquality>]
    type private CheckingType =
        { Identifier: TypeIdentifier
          Attributes: ParsedNodeArray<TypeAttributeNode>
          Extends: ParsedNodeArray<ParsedIdentifier> // TypeIdentifier
          Members: ParsedNodeArray<TypeMemberNode>
          UsedNamespaces: ImmutableArray<ParsedNamespaceName> }

    let private findTypeDeclarations (files: ImmutableArray<ParsedFile>) =
        let rec inner
            (lookup: Dictionary<_, _>)
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
                    declaredTypeNodes.Add({ TypeIdentifier.Name = name.Content; Namespace = currentNamespaceName }, attributes, extends, members)
                | TopLevelNode.UsingNamespace ns ->
                    usings.Add ns
                | TopLevelNode.Error msg ->
                    failwithf "TODO: Handle error %s" msg

            let currentUsingDeclarations = usings.ToImmutable()

            for (id, attributes, extends, members) in declaredTypeNodes do
                // TODO: Handle duplicate type definitions
                lookup.Add(id, { CheckingType.Identifier = id; Attributes = attributes; Extends = extends; Members = members; UsedNamespaces = currentUsingDeclarations })

            for struct(nestedNamespaceName, nested) in nestedNamespaceDeclarations do
                inner lookup nestedNamespaceName currentUsingDeclarations nested

        let lookup = Dictionary<TypeIdentifier, CheckingType>()
        for file in files do inner lookup ImmutableArray.Empty ImmutableArray.Empty file.Nodes
        lookup

    let check files =
        let typeDeclarationLookup = findTypeDeclarations files
        failwith "BAD": CheckedModule
