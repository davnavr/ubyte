namespace MiddleC.Compiler.Semantics

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
    | DuplicateTypeDefinition of FullTypeIdentifier
    | UndefinedTypeIdentifier of TypeIdentifier
    | AmbiguousTypeIdentifier of TypeIdentifier * matches: seq<FullTypeIdentifier>
    | UnknownError of message: string

    override this.ToString() =
        match this with
        | DuplicateTypeDefinition id -> "Duplicate definition of type " + id.ToString()
        | UndefinedTypeIdentifier id -> "The type definition " + id.ToString() + " does not exist"
        | AmbiguousTypeIdentifier(id, matches) ->
            System.Text.StringBuilder(id.ToString())
                .Append(" could refer to any of the following types: ")
                .AppendJoin(", ", matches)
                .ToString()
        | UnknownError message -> message

type SemanticError =
    { Line: uint32
      Column: uint32
      Source: ParsedFile
      Message: SemanticErrorMessage }

type CheckedTypeDefinition =
    private
        { identifier: FullTypeIdentifier
          Source: ParsedFile
          AttributeNodes: ParsedNodeArray<TypeAttributeNode>
          InheritanceNodes: ParsedNodeArray<TypeIdentifier>
          MemberNodes: ParsedNodeArray<TypeMemberNode>
          UsedNamespaces: ImmutableArray<FullNamespaceName>
          [<DefaultValue>] mutable flags: Model.TypeDefinitionFlags }

    member this.Identifier = this.identifier

[<RequireQualifiedAccess>]
type CheckedModule =
    { DefinedTypes: ImmutableArray<CheckedTypeDefinition>
      Errors: ImmutableArray<SemanticError> }

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
                        { identifier = FullTypeIdentifier.create name (FullNamespaceName.toParsedName currentNamespaceName)
                          Source = file
                          AttributeNodes = attributes
                          InheritanceNodes = extends
                          MemberNodes = members
                          UsedNamespaces = usedNamespaceDeclarations }
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
        lookup :> IReadOnlyDictionary<_, _>

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

    type private NamedType = Choice<CheckedTypeDefinition, UByte.Resolver.ResolvedTypeDefinition>

    [<IsReadOnly; Struct; NoComparison; NoEquality>]
    type private CheckedTypeAttributes =
        { Flags: Model.TypeDefinitionFlags
          InheritedTypes: ImmutableArray<NamedType> }

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

    let private checkTypeAttributes errors (declarations: IReadOnlyDictionary<FullTypeIdentifier, _>) namedTypeLookup =
        let lookup = Dictionary<_, CheckedTypeAttributes> declarations.Count
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

            lookup.Add(ty.Identifier, { Flags = flags; InheritedTypes = inheritedTypeBuilder.ToImmutable() })

        lookup

    let private checkTypeMembers errors (declarations: IReadOnlyDictionary<FullTypeIdentifier, _>) =
        ()

    let check files imports =
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

        let typeAttributeLookup = checkTypeAttributes errors declaredTypesLookup namedTypeLookup

        { CheckedModule.DefinedTypes =
            failwith "TODO: Get defined types"
          CheckedModule.Errors =
            if errors.Count <> errors.Capacity
            then errors.ToImmutable()
            else errors.MoveToImmutable() }
