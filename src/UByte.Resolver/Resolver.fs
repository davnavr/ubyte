namespace UByte.Resolver

open System
open System.Collections.Immutable
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Text

open UByte.Format.Model

[<AutoOpen>]
module private Helpers =
    let inline isFlagSet flag value = value &&& flag = flag

    let createIndexedLookup (count: int32) initializer =
        let lookup = Dictionary<Index<_>, _> count
        fun i ->
            match lookup.TryGetValue i with
            | true, existing -> existing
            | false, _ ->
                let value = initializer i
                lookup.Add(i, value)
                value

    let inline (|ItemIndex|) (Index i) = Checked.int32 i

    let createDefinitionOrImportLookup definedCount importCount definedInitializer importInitializer =
        createIndexedLookup (definedCount + importCount) <| fun (ItemIndex i as index) ->
            if i < importCount
            then importInitializer i index
            else definedInitializer (i - importCount) index

[<Sealed>]
type ResolvedModule =
    val private source : Module
    val private importedModuleLookup : ModuleIndex -> ResolvedModule
    val private moduleResolvedEvent : Event<ResolvedModule>
    val private typeDefinitionLookup : TypeDefinitionIndex -> ResolvedTypeDefinition
    val private typeResolvedEvent : Event<ResolvedTypeDefinition>
    val private definedMethodLookup : MethodIndex -> ResolvedMethod
    val private methodResolvedEvent : Event<ResolvedMethod>
    val private definedFieldLookup : FieldIndex -> ResolvedField
    val private fieldResolvedEvent : Event<ResolvedField>
    val private typeNameLookup : Dictionary<struct(string * string), ResolvedTypeDefinition>

    member this.Identifier = this.source.Header.Module
    member this.Name = this.Identifier.ModuleName.ToString()

    [<CLIEvent>] member this.ModuleResolved = this.moduleResolvedEvent.Publish
    [<CLIEvent>] member this.TypeResolved = this.typeResolvedEvent.Publish
    [<CLIEvent>] member this.MethodResolved = this.methodResolvedEvent.Publish
    [<CLIEvent>] member this.FieldResolved = this.fieldResolvedEvent.Publish

    member this.IdentifierAt index = this.source.Identifiers.[index]

    member this.NamespaceAt(ItemIndex i: NamespaceIndex): string =
        this.source.Namespaces.[i] |> Seq.map this.IdentifierAt |> String.concat "::" // TODO: Cache namespaces

    override this.ToString() = sprintf "(%s, v%O)" this.Name this.Identifier.Version

and [<Sealed>] ResolvedTypeDefinition =
    val private rmodule : ResolvedModule
    val private source : TypeDefinition
    val private index : TypeDefinitionIndex
    val private specifiedInheritedTypes : Lazy<ImmutableArray<ResolvedTypeDefinition>>
    val private vtable : Lazy<Dictionary<ResolvedMethod, ResolvedMethod>>

    member this.DeclaringModule = this.rmodule
    member this.BaseTypes = this.specifiedInheritedTypes.Value
    member this.VTable = this.vtable.Value :> IReadOnlyDictionary<_, _>
    member this.Name = this.DeclaringModule.IdentifierAt this.source.TypeName
    member this.Namespace = this.DeclaringModule.NamespaceAt this.source.TypeNamespace

    override this.ToString() =
        StringBuilder(this.DeclaringModule.ToString())
            .Append(if this.Namespace.Length > 0 then "::" + this.Namespace else String.Empty)
            .Append("::")
            .Append(this.Name)
            .ToString()

and [<Sealed>] ResolvedMethod =
    val private owner : ResolvedTypeDefinition
    val private source : Method
    val private index : MethodIndex

    member this.DeclaringType = this.owner
    member this.DeclaringModule = this.DeclaringType.DeclaringModule
    member this.Name = this.DeclaringModule.IdentifierAt this.source.MethodName
    member this.Flags = this.source.MethodFlags
    member this.IsInstance = isFlagSet MethodFlags.Instance this.Flags
    member this.IsConstructor = isFlagSet MethodFlags.ConstructorMask this.Flags
    member this.IsVirtual = isFlagSet MethodFlags.Virtual this.Flags
    member this.IsExternal = match this.source.Body with | MethodBody.External _ -> true | _ -> false

    override this.ToString() =
        let name = this.Name
        let t = System.Text.StringBuilder(this.DeclaringType.ToString()).Append('.')

        if String.IsNullOrEmpty name then
            let (Index i) = this.index
            t.Append('<')
                .Append(if this.IsConstructor then "constructor" else "method")
                .Append('-')
                .Append(i)
                .Append('>')
        else
            t.Append name
        |> ignore

        // TODO: Include argument types in string.

        t.ToString()

and [<Sealed>] ResolvedField =
    val private parent : ResolvedTypeDefinition
    val private source : Field
    val private index : FieldIndex

    member this.DeclaringType = this.parent

type ResolvedMethod with
    new (rt, source, index) =
        { owner = rt
          source = source
          index = index }

type ResolvedField with
    new (rt, source, index) =
        { parent = rt
          source = source
          index = index }

[<Sealed>]
type ModuleNotFoundException (identifier: ModuleIdentifier, message) =
    inherit Exception(message)

    member _.Identifier = identifier

[<Sealed>]
type TypeNotFoundException (m: ResolvedModule, typeNamespace, typeName, message: string) =
    inherit Exception(message)

    member _.TypeNamespace: string = typeNamespace
    member _.TypeName: string = typeName
    member _.Module = m

type ResolvedModule with
    member this.ModuleAt index = this.importedModuleLookup index
    member this.TypeAt index = this.typeDefinitionLookup index
    member this.MethodAt index = this.definedMethodLookup index
    member this.FieldAt index = this.definedFieldLookup index
    member this.TypeSignatureAt(ItemIndex i: TypeSignatureIndex) = this.source.TypeSignatures.[i]
    member this.MethodSignatureAt(ItemIndex i: MethodSignatureIndex) = this.source.MethodSignatures.[i]

    member this.FindType(typeNamespace: string, typeName: string): ResolvedTypeDefinition =
        let key = struct(typeNamespace, typeName)
        match this.typeNameLookup.TryGetValue key with
        | false, _ ->
            let types = this.source.Definitions.DefinedTypes
            let mutable result, i = ValueNone, this.source.Imports.ImportedTypes.Length

            while i < types.Length && result.IsNone do
                let t = types.[i]

                if
                    t.TypeVisibility <= VisibilityFlags.Public &&
                    this.NamespaceAt t.TypeNamespace = typeNamespace &&
                    this.IdentifierAt t.TypeName = typeName
                then
                    let tindex = TypeDefinitionIndex.Index(Checked.uint32 i)
                    let init = this.TypeAt tindex
                    this.typeNameLookup.[key] <- init
                    result <- ValueSome init

                i <- Checked.(+) 1 i

            match result with
            | ValueSome t -> t
            | ValueNone ->
                TypeNotFoundException (
                    this,
                    typeNamespace,
                    typeName, sprintf "Unable to find type %s %s" typeNamespace typeName
                )
                |> raise
        | true, existing -> existing

type ResolvedTypeDefinition with
    new (rm, source, index) =
        { rmodule = rm
          source = source
          index = index
          specifiedInheritedTypes =
            let { TypeDefinition.InheritedTypes = indices } = source
            lazy
                let mutable inherited = Array.zeroCreate indices.Length
                for i = 0 to inherited.Length - 1 do inherited.[i] <- rm.TypeAt indices.[i]
                Unsafe.As<ResolvedTypeDefinition[], ImmutableArray<ResolvedTypeDefinition>> &inherited
          vtable =
            let { TypeDefinition.VTable = overrides } = source
            lazy
                let lookup = Dictionary overrides.Length
                let inline (|Method|) mindex = rm.MethodAt mindex
                for i = 0 to overrides.Length - 1 do
                    let { MethodOverride.Declaration = Method decl; Implementation = Method impl } = overrides.[i]
                    // TODO: Check that owner of decl is an inherited type
                    // TODO: Check that impl is owned by this type
                    // TODO: Check that decl and impl are different
                    lookup.Add(decl, impl)
                lookup }

type ResolvedMethod with
    member this.Visibility = this.source.MethodVisibility
    member this.Signature = this.DeclaringModule.MethodSignatureAt this.source.Signature

type ResolvedField with
    member this.DeclaringModule = this.DeclaringType.DeclaringModule
    member this.Name = this.DeclaringModule.IdentifierAt this.source.FieldName
    member this.Flags = this.source.FieldFlags
    member this.Visibility = this.source.FieldVisibility
    member this.IsMutable = isFlagSet FieldFlags.Mutable this.Flags
    member this.IsStatic = isFlagSet FieldFlags.Static this.Flags

type ResolvedTypeDefinition with
    member this.FindMethod name = // TODO: Figure out if methods defined in inherited class(es), note that this is used to resolve method references.
        let methodis = this.source.Methods
        let mutable result, i = ValueNone, 0

        while i < methodis.Length && result.IsNone do
            let m = this.DeclaringModule.MethodAt methodis.[i]
            if m.Visibility <= VisibilityFlags.Public && m.Name = name then
                result <- ValueSome m
            i <- Checked.(+) i 1

        match result with
        | ValueSome m -> m
        | ValueNone -> failwithf "TODO: Method not found %s" name

type ResolvedModule with
    new (source, importer) as this =
        { source = source
          importedModuleLookup =
            let imports = source.Imports.ImportedModules
            createIndexedLookup imports.Length <| fun (Index i) ->
                if i = 0u then
                    this
                else
                    let rm = importer imports.[Checked.int32 i - 1]
                    this.moduleResolvedEvent.Trigger rm
                    rm
          moduleResolvedEvent = Event<_>()
          typeDefinitionLookup =
            let imports = source.Imports.ImportedTypes
            let definitions = source.Definitions.DefinedTypes
            createDefinitionOrImportLookup definitions.Length imports.Length
                (fun i i' ->
                    let rt = ResolvedTypeDefinition(this, definitions.[i], i')
                    this.typeResolvedEvent.Trigger rt
                    rt)
                (fun i _ ->
                    let t = imports.[Checked.int32 i]
                    let owner = this.ModuleAt t.Module
                    owner.FindType(this.NamespaceAt t.TypeNamespace, this.IdentifierAt t.TypeName))
          typeResolvedEvent = Event<_>()
          definedMethodLookup =
            let imports = source.Imports.ImportedMethods
            let definitions = source.Definitions.DefinedMethods
            createDefinitionOrImportLookup definitions.Length imports.Length
                (fun i index ->
                    let method = definitions.[i]
                    let rm = ResolvedMethod(this.TypeAt method.MethodOwner, method, index)
                    this.methodResolvedEvent.Trigger rm
                    rm)
                (fun i _ ->
                    let m = imports.[Checked.int32 i]
                    let owner = this.TypeAt m.MethodOwner
                    owner.FindMethod(this.IdentifierAt m.MethodName))
          methodResolvedEvent = Event<_>()
          definedFieldLookup =
            let owners = source.Definitions.DefinedTypes
            let fields = source.Definitions.DefinedFields
            createIndexedLookup fields.Length <| fun (Index i as i') -> // TODO: Support lookup for field imports in resolver.
                let field = fields.[Checked.int32 i]
                //let (Index owner) = f.FieldOwner
                //let n = owners.[Checked.int32 owner].Fields.IndexOf i' // Won't work for inherited types
                let rf = ResolvedField(this.TypeAt field.FieldOwner, field, i')
                this.fieldResolvedEvent.Trigger rf
                rf
          fieldResolvedEvent = Event<_>()
          typeNameLookup = Dictionary() }

    member this.EntryPoint =
        match this.source.EntryPoint with
        | ValueSome eindex -> ValueSome(this.MethodAt eindex)
        | ValueNone -> ValueNone
