namespace MiddleC.Compiler.Semantics

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open MiddleC.Compiler.Parser

open UByte.Format

type SemanticErrorMessage =
    | DuplicateTypeDefinition of MiddleC.Compiler.Parser.TypeIdentifier
    | UnknownError of message: string

    override this.ToString() =
        match this with
        | DuplicateTypeDefinition id -> "Duplicate definition of type " + id.ToString()
        | UnknownError message -> message

type SemanticError =
    { Line: uint32
      Column: uint32
      Source: ParsedFile
      Message: SemanticErrorMessage }

[<RequireQualifiedAccess>]
type CheckedModule =
    { Errors: ImmutableArray<SemanticError> } 

[<RequireQualifiedAccess>]
module TypeChecker =
    [<NoComparison; NoEquality>]
    type private CheckingType =
        { Identifier: TypeIdentifier
          Source: ParsedFile
          Attributes: ParsedNodeArray<TypeAttributeNode>
          InheritedTypes: ParsedNodeArray<TypeIdentifier>
          Members: ParsedNodeArray<TypeMemberNode>
          UsedNamespaces: ImmutableArray<ParsedNamespaceName> }

    let private addErrorMessage (errors: ImmutableArray<_>.Builder) (node: ParsedNode<_>) source message =
        errors.Add { Line = node.Line; Column = node.Column; Source = source; Message = message }

    let private findTypeDeclarations errors (files: ImmutableArray<ParsedFile>) =
        let rec inner
            errors
            file
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
                    declaredTypeNodes.Add <| fun usedNamespaceDeclarations ->
                        { CheckingType.Identifier =
                            { TypeIdentifier.Name = name
                              TypeIdentifier.Namespace = currentNamespaceName }
                          Source = file
                          Attributes = attributes
                          InheritedTypes = extends
                          Members = members
                          UsedNamespaces = usedNamespaceDeclarations }
                | TopLevelNode.UsingNamespace ns ->
                    usings.Add ns
                | TopLevelNode.Error msg -> // TODO: Error nodes should be treated as UserState in parser stage, TypeChecker should have to deal with parser errors.
                    addErrorMessage errors node file (UnknownError msg)

            let currentUsingDeclarations = usings.ToImmutable()

            for declaredTypeBuilder in declaredTypeNodes do
                let ty = declaredTypeBuilder currentUsingDeclarations
                if not(lookup.TryAdd(ty.Identifier, ty)) then
                    addErrorMessage errors ty.Identifier.Name ty.Source (DuplicateTypeDefinition ty.Identifier)

            for struct(nestedNamespaceName, nested) in nestedNamespaceDeclarations do
                inner errors file lookup nestedNamespaceName currentUsingDeclarations nested

        let lookup = Dictionary<TypeIdentifier, CheckingType>()
        for file in files do inner errors file lookup ImmutableArray.Empty ImmutableArray.Empty file.Nodes
        lookup :> IReadOnlyDictionary<_, _>

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

    type private NamedType = Choice<CheckingType, UByte.Resolver.ResolvedTypeDefinition>

    [<IsReadOnly; Struct; NoComparison; NoEquality>]
    type private CheckedTypeAttributes =
        { Flags: Model.TypeDefinitionFlags
          InheritedTypes: ImmutableArray<NamedType> }

    let private checkTypeAttributes errors (declarations: IReadOnlyDictionary<TypeIdentifier, _>) (namedTypeLookup: _ -> _) =
        let lookup = Dictionary<_, CheckedTypeAttributes> declarations.Count
        let inheritedTypeBuilder = ImmutableArray.CreateBuilder<NamedType>()

        for (ty: CheckingType) in declarations.Values do
            let mutable flags = Model.TypeDefinitionFlags.Final

            for attr in ty.Attributes do
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

            for inheritedTypeName in ty.InheritedTypes do
                match namedTypeLookup inheritedTypeName with
                | Ok ty ->
                    inheritedTypeBuilder.Add ty
                | Error message ->
                    addErrorMessage errors inheritedTypeName ty.Source message

            lookup.Add(ty.Identifier, { Flags = flags; InheritedTypes = inheritedTypeBuilder.ToImmutable() })

        lookup

    let check files imports =
        let errors = ImmutableArray.CreateBuilder()
        let declaredTypesLookup = findTypeDeclarations errors files

        let typeImportLookup = importedTypeLookup imports

        let namedTypeLookup ({ ParsedNode.Content = name } as id): Result<NamedType, _> =
            match declaredTypesLookup.TryGetValue name with
            | true, declared ->
                Ok(Choice1Of2 declared)
            | false, _ ->
                match typeImportLookup name with
                | ValueSome imported ->
                    Ok(Choice2Of2 imported)
                | ValueNone ->
                    raise(NotImplementedException("TODO: Handle errors for when type name not found " + name.ToString()))

        let typeAttributeLookup = checkTypeAttributes errors declaredTypesLookup namedTypeLookup

        failwith "BAD"

        { CheckedModule.Errors =
            if errors.Count <> errors.Capacity
            then errors.ToImmutable()
            else errors.MoveToImmutable() }
