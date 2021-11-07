namespace UByte.Resolver

open System
open System.Collections.Immutable
open System.Collections.Generic

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
    val private typeDefinitionLookup : TypeDefinitionIndex -> ResolvedTypeDefinition
    val private definedMethodLookup : MethodIndex -> ResolvedMethod
    val private definedFieldLookup : FieldIndex -> ResolvedField
    val private typeNameLookup : Dictionary<struct(string * string), ResolvedTypeDefinition>

and [<Sealed>] ResolvedTypeDefinition =
    val DeclaringModule : ResolvedModule
    val private source : TypeDefinition
    val private index : TypeDefinitionIndex

and [<Sealed>] ResolvedMethod =
    val DeclaringType : ResolvedTypeDefinition
    val private source : Method
    val private index : MethodIndex

and [<Sealed>] ResolvedField =
    val DeclaringType : ResolvedTypeDefinition
    val private source : Field
    val private index : FieldIndex

type ResolvedTypeDefinition with
    new (rm, source, index) =
        { DeclaringModule = rm
          source = source
          index = index }

type ResolvedMethod with
    new (rt, source, index) =
        { DeclaringType = rt
          source = source
          index = index }

type ResolvedField with
    new (rt, source, index) =
        { DeclaringType = rt
          source = source
          index = index }

[<Sealed>]
type ModuleNotFoundException (name: ModuleIdentifier, message) =
    inherit Exception(message)

    member _.Name = name

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
    member this.IdentifierAt index = this.source.Identifiers.[index]
    member this.MethodSignatureAt(ItemIndex i: MethodSignatureIndex) = this.source.MethodSignatures.[i]

    member this.NamespaceAt(ItemIndex i: NamespaceIndex): string =
        this.source.Namespaces.[i] |> Seq.map this.IdentifierAt |> String.concat "::" // TODO: Cache namespaces

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

type ResolvedMethod with
    member this.DeclaringModule = (this.DeclaringType : ResolvedTypeDefinition).DeclaringModule
    member this.Visibility = this.source.MethodVisibility
    member this.DeclaringType = this.DeclaringModule.TypeAt this.source.MethodOwner
    member this.Signature = this.DeclaringModule.MethodSignatureAt this.source.Signature

type ResolvedField with
    member this.DeclaringModule = this.DeclaringType.DeclaringModule
    member this.IsMutable = isFlagSet FieldFlags.Mutable this.source.FieldFlags
    member this.IsStatic = isFlagSet FieldFlags.Static this.source.FieldFlags

type ResolvedModule with
    new (source, resolver) as this =
        { source = source
          importedModuleLookup =
            let imports = source.Imports.ImportedModules
            createIndexedLookup imports.Length <| fun (Index i) ->
                if i = 0u
                then this
                else resolver imports.[Checked.int32 i - 1]
          typeDefinitionLookup =
            let imports = source.Imports.ImportedTypes
            let definitions = source.Definitions.DefinedTypes
            createDefinitionOrImportLookup definitions.Length imports.Length
                (fun i i' -> ResolvedTypeDefinition(this, definitions.[i], i'))
                (fun i _ ->
                    let t = imports.[Checked.int32 i]
                    let owner = this.ModuleAt t.Module
                    owner.FindType(this.NamespaceAt t.TypeNamespace, this.IdentifierAt t.TypeName)) 
          definedMethodLookup =
            let imports = source.Imports.ImportedMethods
            let definitions = source.Definitions.DefinedMethods
            createDefinitionOrImportLookup definitions.Length imports.Length
                (fun i index ->
                    let method = definitions.[i]
                    ResolvedMethod(this.TypeAt method.MethodOwner, method, index))
                (fun i _ ->
                    let m = imports.[Checked.int32 i]
                    let owner = this.TypeAt m.MethodOwner
                    owner.FindMethod(rm.IdentifierAt m.MethodName))
          definedFieldLookup =
            let owners = source.Definitions.DefinedTypes
            let fields = source.Definitions.DefinedFields
            createIndexedLookup fields.Length <| fun (Index i as i') ->
                let field = fields.[Checked.int32 i]
                //let (Index owner) = f.FieldOwner
                //let n = owners.[Checked.int32 owner].Fields.IndexOf i' // Won't work for inherited types
                ResolvedField(this.TypeAt field.FieldOwner, field, i')
          typeNameLookup = Dictionary() }
