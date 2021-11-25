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
    let methodBodyLookup = Dictionary<_, struct(CodeIndex * _)>()
    let definedTypeIndices = Dictionary<CheckedTypeDefinition, uint32> types.Length
    let definedTypeList = List types.Length

    for definition in types do
        //for field in definition.Fields do

        for method in definition.Methods do
            definedMethodIndices.Add(method, uint32 definedMethodIndices.Count)

            match method.Body with
            | CheckedMethodBody.Defined statements ->
                methodBodyLookup.Add(method, struct(CodeIndex.Index(uint32 methodBodyLookup.Count), statements))
            | CheckedMethodBody.External _ -> ()

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

/// Generates the corresponding byte code for a given expression.
let rec private writeExpressionCode
    nextTemporaryIndex
    dataIndexLookup
    typeSignatureLookup
    (instructions: ImmutableArray<_>.Builder)
    (expression: TypedExpression)
    : ImmutableArray<RegisterIndex> =
    let inline writeOneRegister() = ImmutableArray.Create(item = nextTemporaryIndex())

    match expression.Expression with
    | CheckedExpression.LiteralSignedInteger value ->
        match expression.Type with
        | CheckedType.ValueType(CheckedValueType.Primitive ptype) ->
            // TODO: Ensure Const_i can work with 64-bit integer literals.
            instructions.Add(InstructionSet.Const_i(ptype, Checked.int32 value))
            writeOneRegister()
        | _ ->
            raise(NotImplementedException(sprintf "Cannot generate integer literal of type %O" expression.Type))
    | CheckedExpression.NewArray(etype, elements) ->
        match etype with
        | CheckedElementType.ValueType(CheckedValueType.Primitive(PrimitiveType.Char32 | PrimitiveType.U32) as vtype) ->
            let mutable bytes, isConstantExpression, constantValueCount = Array.zeroCreate(4 * elements.Length), true, 0
            for e in elements do
                match e.Expression with
                | CheckedExpression.LiteralUnsignedInteger value ->
                    // TODO: Allow selection of endianness for these constant strings.
                    let written = BitConverter.TryWriteBytes(Span(bytes, constantValueCount * 4, bytes.Length - constantValueCount * 4), uint32 value)
                    assert written
                    constantValueCount <- constantValueCount + 1
                | _ ->
                    raise(NotImplementedException("Unsupported array element " + e.ToString()))

            if isConstantExpression then
                instructions.Add(InstructionSet.Obj_arr_const(typeSignatureLookup(CheckedType.ValueType vtype), dataIndexLookup (Unsafe.As<byte[], ImmutableArray<byte>> &bytes)))
                writeOneRegister()
            else
                raise(NotImplementedException "NON-DATA arrays not yet implemented")
        | _ ->
            raise(NotImplementedException(sprintf "Code generation for new array containing %O is not yet implemented" etype))
    | _ ->
        raise(NotImplementedException(sprintf "Code generation is not yet implemented for %O" expression))

let private writeMethodBodies
    dataIndexLookup
    typeSignatureLookup
    (methodBodyLookup: Dictionary<CheckedMethod, struct(CodeIndex * ImmutableArray<_>)>) =
    let mutable bodies = Array.zeroCreate methodBodyLookup.Count

    let localRegisterLookup = Dictionary<_, struct(CodeBlockIndex * TemporaryIndex)>()
    let blocks = ImmutableArray.CreateBuilder<CodeBlock>()

    let blockLocalMap = ImmutableArray.CreateBuilder<struct(_ * _)>()
    let instructions = ImmutableArray.CreateBuilder<InstructionSet.Instruction>()
    let mutable nextTemporaryIndex = 0u

    let createTemporaryRegister() =
        let index = RegisterIndex.Index nextTemporaryIndex
        nextTemporaryIndex <- nextTemporaryIndex + 1u
        index

    let inline writeCurrentBlock () =
        if blocks.Count > 0 then
            blocks.Add
                { CodeBlock.ExceptionHandler = ValueNone
                  CodeBlock.Locals = blockLocalMap.ToImmutable()
                  CodeBlock.Instructions = instructions.ToImmutable() }

        blockLocalMap.Clear()
        instructions.Clear()
        nextTemporaryIndex <- 0u

    for KeyValue(method, struct(Index index, statements)) in methodBodyLookup do
        localRegisterLookup.Clear()
        blocks.Clear()

        for statement in statements do
            match statement with
            | CheckedStatement.Expression value ->
                writeExpressionCode createTemporaryRegister dataIndexLookup typeSignatureLookup instructions value |> ignore
            | CheckedStatement.LocalDeclaration(_, name, _, value) ->
                let values = writeExpressionCode createTemporaryRegister dataIndexLookup typeSignatureLookup instructions value
                if values.Length <> 1 then
                    raise(NotImplementedException(sprintf "Code generation for local variable containing %i values is not yet implemented" values.Length))

                let (Index i) = values.[0]
                if i >= nextTemporaryIndex then
                    invalidOp "Expected temporary register but expression code returned a local or argument register"
                let lindex = TemporaryIndex.Index i

                blockLocalMap.Add(struct(lindex, LocalIndex.Index(uint32 localRegisterLookup.Count)))
                localRegisterLookup.Add(name, struct(CodeBlockIndex.Index(uint32 blocks.Count), lindex))
            | CheckedStatement.Return values ->
                if values.IsDefaultOrEmpty then
                    instructions.Add(InstructionSet.Ret ImmutableArray.Empty)
                elif values.Length = 1 then
                    let mutable returns = Array.zeroCreate values.Length
                    for i = 0 to returns.Length - 1 do
                        let expressions = writeExpressionCode createTemporaryRegister dataIndexLookup typeSignatureLookup instructions values.[i]
                        if expressions.Length <> 1 then
                            raise(NotImplementedException "TODO: Handle weird number of return values")
                        returns.[i] <- expressions.[0]
                    instructions.Add(InstructionSet.Ret(Unsafe.As<RegisterIndex[], ImmutableArray<RegisterIndex>> &returns))
                else
                    raise(NotImplementedException "Code generation for multiple return values is not yet implemented")
            | CheckedStatement.Empty -> instructions.Add InstructionSet.Nop

        writeCurrentBlock() // Writes the last block, if any

        bodies.[int32 index] <-
            { Code.LocalCount = uint32 localRegisterLookup.Count
              Code.Blocks = blocks.ToImmutable() }

    Unsafe.As<Code[], ImmutableArray<Code>> &bodies

let write (mdl: CheckedModule) =
    if not mdl.Errors.IsDefaultOrEmpty then raise(ArgumentException "The module must not contain any errors")

    // TODO: Module import array should be a ImmutableArray.Builder since more modules might get imported later.
    let struct(moduleImportArray, moduleImportLookup) = writeModuleImports mdl.ImportedModules

    let struct(identifiers, identifierIndexLookup: _ -> IdentifierIndex) =
        createIndexedLookup StringComparer.Ordinal id

    let struct(namespaces, namespaceIndexLookup: _ -> NamespaceIndex) =
        createIndexedLookup
            EqualityComparer.Default
            (fun (ns: FullNamespaceName) ->
                let names = FullNamespaceName.toParsedName ns
                let mutable indices = Array.zeroCreate names.Length
                for i = 0 to indices.Length - 1 do indices.[i] <- identifierIndexLookup(names.[0].Content.ToString())
                Unsafe.As<IdentifierIndex[], ImmutableArray<IdentifierIndex>> &indices)

    let struct (data, dataIndexLookup: ImmutableArray<byte> -> DataIndex) =
        createIndexedLookup EqualityComparer.Default id

    let typeSignaturesReference = ref Unchecked.defaultof<_>

    let struct(tsignatures, typeSignatureLookup: CheckedType -> TypeSignatureIndex) =
        createIndexedLookup
            EqualityComparer.Default
            (function
            | CheckedType.ValueType(CheckedValueType.Primitive prim) ->
                AnyType.primitive prim
            | CheckedType.ReferenceType rtype ->
                match rtype with
                | CheckedReferenceType.Any -> ReferenceType.Any
                | CheckedReferenceType.Array etype ->
                    failwith "TODO: Get element type"
                |> AnyType.ReferenceType
            | CheckedType.Void ->
                invalidOp "Cannot create a type signature for void")

    typeSignaturesReference.Value <- typeSignatureLookup

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
    let code = writeMethodBodies dataIndexLookup typeSignatureLookup methodBodyLookup

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
                    | CheckedMethodBody.Defined _ ->
                        let struct(index, _) = methodBodyLookup.[method]
                        MethodBody.Defined index
                    | CheckedMethodBody.External(name, library) ->
                        MethodBody.External(identifierIndexLookup library, identifierIndexLookup name) }
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
      Module.Data = data.ToImmutable()
      Module.Code = code
      Module.EntryPoint = ValueOption.map methodDefinitionIndex mdl.EntryPoint
      Module.Debug = () }
