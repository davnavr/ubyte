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
    let mutable imports = Array.zeroCreate modules.Length
    for i = 0 to imports.Length - 1 do imports.[i] <- modules.[i].Identifier
    Unsafe.As<ModuleIdentifier[], ImmutableArray<ModuleIdentifier>> &imports

let private writeTypeDefinitions identifierIndexLookup namespaceIndexLookup (types: ImmutableArray<CheckedTypeDefinition>) =
    () // TODO: Assign indices to fields and methods

let write (mdl: CheckedModule) =
    if not mdl.Errors.IsDefaultOrEmpty then raise(ArgumentException "The module must not contain any errors")

    let moduleImportArray = writeModuleImports mdl.ImportedModules

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

    let struct(msignatures, methodSignatureLookup: CheckedMethod -> MethodSignatureIndex) =
        createIndexedLookup
            EqualityComparer.Default // TODO: Compare method signatures instead to reduce number of method signatures in output.
            (fun method ->
                { MethodSignature.ReturnTypes =
                    let mutable rtypes = Array.zeroCreate method.ReturnTypes.Length
                    for i = 0 to rtypes.Length - 1 do rtypes.[i] <- typeSignatureLookup method.ReturnTypes.[i]
                    Unsafe.As<TypeSignatureIndex[], ImmutableArray<TypeSignatureIndex>> &rtypes
                  MethodSignature.ParameterTypes =
                    let mutable ptypes = Array.zeroCreate method.Parameters.Length
                    for i = 0 to ptypes.Length - 1 do ptypes.[i] <- typeSignatureLookup(method.Parameters.ItemRef(i).Type)
                    Unsafe.As<TypeSignatureIndex[], ImmutableArray<TypeSignatureIndex>> &ptypes })

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
        { ModuleImports.ImportedModules = moduleImportArray }
      }
