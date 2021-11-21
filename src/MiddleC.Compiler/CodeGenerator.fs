module MiddleC.Compiler.CodeGenerator

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open MiddleC.Compiler.Semantics

open UByte.Format.Model

let private createIndexedLookup comparer valueFactory =
    let lookup = Dictionary<_, Index<_>>(comparer = comparer)
    let items = ImmutableArray.CreateBuilder()
    let itemIndexLookup key =
        match lookup.TryGetValue key with
        | true, index -> index
        | false, _ ->
            let index = Index(uint32 lookup.Count)
            lookup.Add(key, index)
            items.Add(valueFactory key)
            index
    struct(items, itemIndexLookup)

let private writeModuleImports (modules: ImmutableArray<UByte.Resolver.ResolvedModule>) =
    let lookup = Dictionary<_, ModuleIndex> modules.Length
    let mutable imports = Array.zeroCreate modules.Length
    for i = 0 to imports.Length - 1 do
        imports.[i] <- modules.[i].Identifier
        lookup.Add(modules.[i], ModuleIndex.Index(1u + uint32 i))
    struct(Unsafe.As<ModuleIdentifier[], ImmutableArray<ModuleIdentifier>> &imports, lookup)

let private writeTypeDefinitions
    identifierIndexLookup
    namespaceIndexLookup
    typeImportLookup
    (types: ImmutableArray<CheckedTypeDefinition>)
    =
    //let definedFieldIndices = Dictionary<CheckedField, uint32>()
    let definedMethodIndices = Dictionary<CheckedMethod, uint32>()
    let methodBodyLookup = Dictionary<CheckedMethod, CodeIndex>()
    let definedTypeIndices = Dictionary<CheckedTypeDefinition, uint32> types.Length
    let definedTypeList = List types.Length

    for definition in types do
        //for field in definition.Fields do

        for method in definition.Methods do
            definedMethodIndices.Add(method, uint32 definedMethodIndices.Count)

            match method.Body with
            | CheckedMethodBody.Defined _ ->
                methodBodyLookup.Add(method, CodeIndex.Index(uint32 methodBodyLookup.Count))

        definedTypeIndices.Add(definition, uint32 definedTypeIndices.Count)

        let typeDefinitionName = identifierIndexLookup(definition.Identifier.Name.Content.ToString())
        let typeDefinitionNamespace = namespaceIndexLookup definition.Identifier.Namespace

        definedTypeList.Add <| fun typeImportCount fieldImportCount methodImportCount ->
            { TypeDefinition.TypeName = typeDefinitionName
              TypeDefinition.TypeNamespace = typeDefinitionNamespace
              TypeDefinition.TypeVisibility = definition.Visibility
              TypeDefinition.TypeFlags = definition.Flags
              TypeDefinition.TypeLayout = TypeDefinitionLayout.Unspecified
              TypeDefinition.TypeParameters = ImmutableArray.Empty
              TypeDefinition.InheritedTypes =
                let mutable inherited = Array.zeroCreate definition.InheritedTypes.Length
                for i = 0 to inherited.Length - 1 do
                    inherited.[i] <-
                        match definition.InheritedTypes.[i] with
                        | Choice1Of2 defined -> TypeDefinitionIndex.Index(typeImportCount + uint32 definedTypeIndices.[defined])
                        | Choice2Of2 import -> typeImportLookup import
                Unsafe.As<TypeDefinitionIndex[], ImmutableArray<TypeDefinitionIndex>> &inherited
              TypeDefinition.TypeAnnotations = ImmutableArray.Empty
              TypeDefinition.Fields = ImmutableArray.Empty
              TypeDefinition.Methods =
                let mutable methods = Array.zeroCreate definition.Methods.Length
                for i = 0 to methods.Length - 1 do
                    methods.[i] <- MethodIndex.Index(methodImportCount + uint32 definedMethodIndices.[definition.Methods.[i]])
                Unsafe.As<MethodIndex[], ImmutableArray<MethodIndex>> &methods
              TypeDefinition.VTable = ImmutableArray.Empty }

    struct(definedTypeIndices, definedTypeList, definedMethodIndices, methodBodyLookup)

let write (mdl: CheckedModule) =
    if not mdl.Errors.IsDefaultOrEmpty then raise(ArgumentException "The module must not contain any errors")

    // TODO: Module import array should be a ImmutableArray.Builder since more modules might get imported later.
    let struct(moduleImportArray, moduleImportLookup) = writeModuleImports mdl.ImportedModules

    let struct(identifiers, identifierIndexLookup: _ -> IdentifierIndex) =
        createIndexedLookup
            StringComparer.Ordinal
            id

    let struct(namespaces, namespaceIndexLookup: _ -> NamespaceIndex) =
        createIndexedLookup
            EqualityComparer.Default
            (fun (ns: FullNamespaceName) ->
                let names = FullNamespaceName.toParsedName ns
                let mutable indices = Array.zeroCreate names.Length
                for i = 0 to indices.Length - 1 do indices.[i] <- identifierIndexLookup(names.[0].Content.ToString())
                Unsafe.As<IdentifierIndex[], ImmutableArray<IdentifierIndex>> &indices)

    let struct(tsignatures, typeSignatureLookup: CheckedType -> TypeSignatureIndex) =
        createIndexedLookup
            EqualityComparer.Default
            (function
            | CheckedType.ValueType(CheckedValueType.Primitive prim) ->
                AnyType.primitive prim)

    let struct(msignatures, methodSignatureLookup: CheckedMethodSignature -> MethodSignatureIndex) =
        createIndexedLookup
            EqualityComparer.Default // TODO: Compare method signatures instead to reduce number of method signatures in output.
            (fun signature ->
                let inline getSignatureArray (types: ImmutableArray<_>) =
                    let mutable types' = Array.zeroCreate types.Length
                    for i = 0 to types'.Length - 1 do types'.[i] <- typeSignatureLookup types.[i]
                    Unsafe.As<TypeSignatureIndex[], ImmutableArray<TypeSignatureIndex>> &types'

                { MethodSignature.ReturnTypes = getSignatureArray signature.ReturnTypes
                  MethodSignature.ParameterTypes = getSignatureArray signature.ParameterTypes })

    let struct(importedTypeDefinitions, typeImportLookup: _ -> TypeDefinitionIndex) =
        createIndexedLookup
            EqualityComparer.Default
            (fun (ty: UByte.Resolver.ResolvedTypeDefinition) ->
                { TypeDefinitionImport.Module = moduleImportLookup.[ty.DeclaringModule] // TODO: Fix, can fail if module was not directly imported.
                  TypeName = identifierIndexLookup ty.Name
                  TypeNamespace = failwith "TODO: Get namespace from UByte.Resolver"
                  TypeParameters = 0u })

    let translateExternalType (rm: UByte.Resolver.ResolvedModule) =
        function
        | AnyType.ValueType(ValueType.Primitive prim) -> CheckedType.primitive prim
        | AnyType.SafePointer _ -> raise(NotImplementedException "TODO: Add support for managed pointers")
        | bad -> failwithf "TODO: Implement translation of %A" bad

    let struct(importedMethodDefinitions, methodImportLookup: _ -> MethodIndex) =
        let importedSignatureLookup = Dictionary<_, _>()
        createIndexedLookup
            EqualityComparer.Default
            (fun (method: UByte.Resolver.ResolvedMethod) ->
                { MethodImport.MethodOwner = typeImportLookup method.DeclaringType
                  MethodImport.MethodName = identifierIndexLookup method.Name
                  MethodImport.TypeParameters = 0u
                  MethodImport.Signature =
                    match importedSignatureLookup.TryGetValue method with
                    | true, existing -> existing
                    | false, _ ->
                        let translated =
                            let inline translateMethodType (types: ImmutableArray<TypeSignatureIndex>) =
                                let rm = method.DeclaringModule
                                let mutable mtypes = Array.zeroCreate types.Length
                                for i = 0 to mtypes.Length - 1 do
                                    mtypes.[i] <- translateExternalType rm (rm.TypeSignatureAt types.[i])
                                Unsafe.As<CheckedType[], ImmutableArray<CheckedType>> &mtypes

                            { CheckedMethodSignature.ReturnTypes = translateMethodType method.Signature.ReturnTypes
                              CheckedMethodSignature.ParameterTypes = translateMethodType method.Signature.ParameterTypes }

                        importedSignatureLookup.Add(method, translated)
                        translated
                    |> methodSignatureLookup })

    let struct(definedTypeIndices, definedTypeList, definedMethodIndices, methodBodyLookup) =
        writeTypeDefinitions identifierIndexLookup namespaceIndexLookup typeImportLookup mdl.DefinedTypes

    // Type definitions, method definitions, and field definitions can only be refered to by index after method bodies have been
    // generated.
    let code = ImmutableArray.Empty //writeMethodBodies methodBodyLookup

    let fieldImportCount = uint32
    let methodImportCount = uint32 importedMethodDefinitions.Count
    let typeImportCount = uint32 importedTypeDefinitions.Count

    let typeDefinitionIndex tdef = TypeDefinitionIndex.Index(uint32 importedTypeDefinitions.Count + definedTypeIndices.[tdef])
    let methodDefinitionIndex method = MethodIndex.Index(uint32 importedMethodDefinitions.Count + definedMethodIndices.[method])

    //writeDebugInformation

    let finalTypeDefinitions =
        let mutable definitions = Array.zeroCreate definedTypeList.Count
        for i = 0 to definitions.Length - 1 do
            // TODO: Ensure no changes to import lists are made
            definitions.[i] <- definedTypeList.[i] typeImportCount fieldImportCount methodImportCount
        Unsafe.As<TypeDefinition[], ImmutableArray<TypeDefinition>> &definitions

    let finalMethodDefinitions =
        let mutable definitions = Array.zeroCreate definedMethodIndices.Count
        for KeyValue(method, index) in definedMethodIndices do
            definitions.[int32 index] <-
                { Method.MethodOwner =
                    // TODO: Check import txtmdl example to see that type index of method owner is correct.
                    typeDefinitionIndex method.DeclaringType
                  Method.MethodName = identifierIndexLookup(method.Name.Content.ToString())
                  Method.MethodVisibility = method.Visibility
                  Method.MethodFlags = method.Flags
                  Method.TypeParameters = ImmutableArray.Empty
                  Method.Signature = methodSignatureLookup method.Signature
                  Method.MethodAnnotations = ImmutableArray.Empty
                  Method.Body =
                    match method.Body with
                    | CheckedMethodBody.Defined _ -> MethodBody.Defined methodBodyLookup.[method] }
        Unsafe.As<Method[], ImmutableArray<Method>> &definitions

    { Module.Magic = UByte.Format.Model.magic
      Module.FormatVersion = UByte.Format.Model.currentFormatVersion
      Module.Header =
        { ModuleHeader.Module = mdl.Identifier
          Flags = Unchecked.defaultof<ModuleHeaderFlags>
          PointerSize = PointerSize.Unspecified }
      Module.Identifiers = { IdentifierSection.Identifiers = identifiers.ToImmutable() }
      Module.Namespaces = namespaces.ToImmutable()
      Module.TypeSignatures = tsignatures.ToImmutable()
      Module.MethodSignatures = msignatures.ToImmutable()
      Module.Imports =
        { ModuleImports.ImportedModules = moduleImportArray
          ModuleImports.ImportedTypes = importedTypeDefinitions.ToImmutable()
          ModuleImports.ImportedFields = ImmutableArray.Empty
          ModuleImports.ImportedMethods = importedMethodDefinitions.ToImmutable() }
      Module.Definitions =
        { ModuleDefinitions.DefinedTypes = finalTypeDefinitions
          ModuleDefinitions.DefinedFields = ImmutableArray.Empty
          ModuleDefinitions.DefinedMethods = finalMethodDefinitions }
      // TODO: String literals should be stored as UTF-8 to avoid endianness issues.
      Module.Data = ImmutableArray.Empty
      Module.Code = code
      Module.EntryPoint = ValueOption.map methodDefinitionIndex mdl.EntryPoint
      Module.Debug = () }
