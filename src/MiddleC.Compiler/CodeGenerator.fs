module MiddleC.Compiler.CodeGenerator

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open MiddleC.Compiler.Parser
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

let private writeTypeImports
    identifierIndexLookup
    (moduleImportLookup: Dictionary<_, _>)
    (types: ImmutableArray<UByte.Resolver.ResolvedTypeDefinition>)
    =
    let mutable imports = Array.zeroCreate types.Length
    let lookup = Dictionary<UByte.Resolver.ResolvedTypeDefinition, TypeDefinitionIndex>(capacity = imports.Length)

    for i = 0 to types.Length - 1 do
        let ty = types.[i]
        lookup.Add(ty, TypeDefinitionIndex.Index(uint32 i))
        imports.[i] <-
            { TypeDefinitionImport.Module = moduleImportLookup.[ty.DeclaringModule]
              TypeName = identifierIndexLookup ty.Name
              TypeNamespace = failwith "TODO: Get namespace from UByte.Resolver"
              TypeParameters = 0u }

    struct(Unsafe.As<TypeDefinitionImport[], ImmutableArray<TypeDefinitionImport>> &imports, lookup)

let private writeMethodImports
    identifierIndexLookup
    methodSignatureLookup
    (typeImportLookup: Dictionary<_, _>)
    translateExternalType
    (methods: ImmutableArray<UByte.Resolver.ResolvedMethod>)
    =
    let mutable imports = Array.zeroCreate methods.Length
    let importedSignatureLookup = Dictionary<_, _>()
    let lookup = Dictionary<UByte.Resolver.ResolvedMethod, MethodIndex>(capacity = imports.Length)

    for i = 0 to methods.Length - 1 do
        let method = methods.[i]
        lookup.Add(method, MethodIndex.Index(uint32 i))
        imports.[i] <-
            { MethodImport.MethodOwner = typeImportLookup.[method.DeclaringType]
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
                |> methodSignatureLookup }

    struct(Unsafe.As<MethodImport[], ImmutableArray<MethodImport>> &imports, lookup)

let private assignDefinedTypeIndices typeImportCount (types: ImmutableArray<CheckedTypeDefinition>) =
    let lookup = Dictionary<_, TypeDefinitionIndex>(capacity = types.Length)
    for i = 0 to types.Length - 1 do
        lookup.Add(types.[i], TypeDefinitionIndex.Index(uint32 typeImportCount + uint32 i))
    lookup

let private writeMethodDefinitions
    identifierIndexLookup
    methodSignatureLookup
    (definedTypeIndices: Dictionary<_, _>)
    methodImportCount
    (methods: ImmutableArray<CheckedMethod>)
    =
    let mutable definitions = Array.zeroCreate methods.Length
    let indices = Dictionary<_, MethodIndex>(capacity = methods.Length)
    let bodies = Dictionary<_, struct(CodeIndex * _)>(capacity = methods.Length)

    for i = 0 to definitions.Length - 1 do
        let method = methods.[i]
        let mindex = MethodIndex.Index(uint32 methodImportCount + uint32 i)
        indices.Add(method, mindex)
        definitions.[i] <-
            { Method.MethodOwner = definedTypeIndices.[method.DeclaringType]
              Method.MethodName = identifierIndexLookup(method.Name.Content.ToString())
              Method.MethodVisibility = method.Visibility
              Method.MethodFlags = method.Flags
              Method.TypeParameters = ImmutableArray.Empty
              Method.Signature = methodSignatureLookup method.Signature
              Method.MethodAnnotations = ImmutableArray.Empty
              Method.Body =
                match method.Body with
                | CheckedMethodBody.Defined statements ->
                    let cindex = CodeIndex.Index(uint32 bodies.Count)
                    bodies.Add(method, struct(cindex, statements))
                    MethodBody.Defined cindex
                | CheckedMethodBody.External(name, library) ->
                    MethodBody.External(identifierIndexLookup library, identifierIndexLookup name) }

    struct(Unsafe.As<Method[], ImmutableArray<Method>> &definitions, indices, bodies)

let private writeTypeDefinitions
    identifierIndexLookup
    namespaceIndexLookup
    (typeImportLookup: Dictionary<_, _>)
    (methodDefinitionLookup: Dictionary<_, MethodIndex>)
    (types: Dictionary<CheckedTypeDefinition, TypeDefinitionIndex>)
    =
    let mutable definitions = Array.zeroCreate types.Count

    for KeyValue(ty, Index i) in types do
        definitions.[int32 i] <-
            { TypeDefinition.TypeName = identifierIndexLookup(ty.Identifier.Name.Content.ToString())
              TypeDefinition.TypeNamespace = namespaceIndexLookup ty.Identifier.Namespace
              TypeDefinition.TypeVisibility = ty.Visibility
              TypeDefinition.TypeFlags = ty.Flags
              TypeDefinition.TypeLayout = TypeDefinitionLayout.Unspecified
              TypeDefinition.TypeParameters = ImmutableArray.Empty
              TypeDefinition.InheritedTypes =
                let mutable inherited = Array.zeroCreate ty.InheritedTypes.Length
                for i = 0 to inherited.Length - 1 do
                    inherited.[i] <-
                        match ty.InheritedTypes.[i] with
                        | Choice1Of2 defined -> types.[defined]
                        | Choice2Of2 import -> typeImportLookup.[import]
                Unsafe.As<TypeDefinitionIndex[], ImmutableArray<TypeDefinitionIndex>> &inherited
              TypeDefinition.TypeAnnotations = ImmutableArray.Empty
              TypeDefinition.Fields = ImmutableArray.Empty
              TypeDefinition.Methods =
                let mutable methods = Array.zeroCreate ty.Methods.Length
                for i = 0 to methods.Length - 1 do methods.[i] <- methodDefinitionLookup.[ty.Methods.[i]]
                Unsafe.As<MethodIndex[], ImmutableArray<MethodIndex>> &methods
              TypeDefinition.VTable = ImmutableArray.Empty }

    Unsafe.As<TypeDefinition[], ImmutableArray<TypeDefinition>> &definitions

/// Generates the corresponding byte code for a given expression.
let rec private writeExpressionCode
    nextTemporaryIndex
    dataIndexLookup
    typeSignatureLookup
    (importedMethodIndices: Dictionary<_, MethodIndex>)
    (definedMethodIndices: Dictionary<_, MethodIndex>)
    (localRegisterLookup: Dictionary<_, _>)
    (instructions: ImmutableArray<_>.Builder)
    (expression: TypedExpression)
    : ImmutableArray<RegisterIndex>
    =
    let inline writeOneRegister() = ImmutableArray.Create(item = nextTemporaryIndex())

    let inline writeNestedExpression nested =
        writeExpressionCode
            nextTemporaryIndex
            dataIndexLookup
            typeSignatureLookup
            importedMethodIndices
            definedMethodIndices
            localRegisterLookup
            instructions
            nested

    match expression.Expression with
    | CheckedExpression.LiteralBoolean value ->
        instructions.Add((if value then InstructionSet.Const_true else InstructionSet.Const_false) PrimitiveType.Bool)
        writeOneRegister()
    | CheckedExpression.LiteralSignedInteger value ->
        match expression.Type with
        | CheckedType.ValueType(CheckedValueType.Primitive ptype) ->
            // TODO: Ensure Const_i can work with 64-bit integer literals.
            instructions.Add(InstructionSet.Const_i(ptype, Checked.int32 value))
            writeOneRegister()
        | _ ->
            raise(NotImplementedException(sprintf "Cannot generate integer literal of type %O" expression.Type))
    | CheckedExpression.BinaryOperation(op, x, y) ->
        let xvalues = writeNestedExpression x
        let yvalues = writeNestedExpression y
        let etype = typeSignatureLookup expression.Type
        let count = xvalues.Length + yvalues.Length

        if xvalues.Length + yvalues.Length <> 2 then
            invalidOp(sprintf "Invalid number of values (%i) for binary operation" count)

        let xv, yv = xvalues.[0], yvalues.[0]

        match op with
        | BinaryOperation.Multiplication -> InstructionSet.Mul(InstructionSet.ArithmeticFlags.None, etype, xv, yv)
        | BinaryOperation.Division -> InstructionSet.Div(InstructionSet.ArithmeticFlags.None, etype, xv, yv)
        | BinaryOperation.Modulo -> InstructionSet.Rem(InstructionSet.ArithmeticFlags.None, etype, xv, yv)
        | BinaryOperation.Addition -> InstructionSet.Add(InstructionSet.ArithmeticFlags.None, etype, xv, yv)
        | BinaryOperation.Subtraction -> InstructionSet.Sub(InstructionSet.ArithmeticFlags.None, etype, xv, yv)
        | BinaryOperation.LessThan -> InstructionSet.Cmp_lt(xv, yv)
        | BinaryOperation.LessThanOrEqual -> InstructionSet.Cmp_le(xv, yv)
        | BinaryOperation.GreaterThan -> InstructionSet.Cmp_gt(xv, yv)
        | BinaryOperation.GreaterThanOrEqual -> InstructionSet.Cmp_ge(xv, yv)
        | BinaryOperation.IsEqual -> InstructionSet.Cmp_eq(xv, yv)
        | BinaryOperation.IsNotEqual -> InstructionSet.Cmp_ne(xv, yv)
        | _ -> raise(NotImplementedException("Code generation for the operation is not supported " + op.ToString()))
        |> instructions.Add

        writeOneRegister()
    | CheckedExpression.MethodCall(callee, arguments) -> // TODO: Add support for instance methods and virtual methods.
        let mutable callArgumentRegisters = Array.zeroCreate arguments.Length

        for i = 0 to callArgumentRegisters.Length - 1 do
            let results = writeNestedExpression arguments.[i]

            if results.Length <> 1 then
                invalidOp(sprintf "Invalid number of return values (%i) for method argument" results.Length)

            callArgumentRegisters.[i] <- results.[0]

        let struct(returnValueCount, mindex) =
            match callee with
            | Choice1Of2 dmethod -> dmethod.ReturnTypes.Length, definedMethodIndices.[dmethod]
            | Choice2Of2 imethod -> imethod.Signature.ReturnTypes.Length, importedMethodIndices.[imethod]

        instructions.Add(InstructionSet.Call(InstructionSet.CallFlags.None, mindex, Unsafe.As<RegisterIndex[], ImmutableArray<RegisterIndex>> &callArgumentRegisters))

        match returnValueCount with
        | 0 -> ImmutableArray.Empty
        | _ ->
            let mutable returnValueRegisters = Array.zeroCreate returnValueCount
            for i = 0 to returnValueCount - 1 do returnValueRegisters.[i] <- nextTemporaryIndex()
            Unsafe.As<RegisterIndex[], ImmutableArray<RegisterIndex>> &returnValueRegisters
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
    | CheckedExpression.Local name -> ImmutableArray.Create(item = localRegisterLookup.[name] ())
    | CheckedExpression.ArrayLengthAccess expression ->
        let array = writeNestedExpression expression
        if array.Length <> 1 then invalidOp "Attempt to access array length for multiple values"
        instructions.Add(InstructionSet.Obj_arr_len(InstructionSet.ArithmeticFlags.None, PrimitiveType.UNative, array.[0]))
        writeOneRegister()
    | _ ->
        raise(NotImplementedException(sprintf "Code generation is not yet implemented for %O" expression))

let private writeMethodBodies
    dataIndexLookup
    typeSignatureLookup
    importedMethodIndices
    definedMethodIndices
    (methodBodyLookup: Dictionary<CheckedMethod, struct(CodeIndex * ImmutableArray<_>)>) =
    let mutable bodies = Array.zeroCreate methodBodyLookup.Count

    let localRegisterLookup = Dictionary<_, _>()
    let blocks = ImmutableArray.CreateBuilder<CodeBlock>()

    let blockLocalMap = ImmutableArray.CreateBuilder<struct(_ * _)>()
    let instructions = ImmutableArray.CreateBuilder<InstructionSet.Instruction>()
    let mutable nextTemporaryIndex = 0u

    let createTemporaryRegister() =
        let index = RegisterIndex.Index nextTemporaryIndex
        nextTemporaryIndex <- nextTemporaryIndex + 1u
        index

#if DEBUG
    let writeCurrentBlock () =
#else
    let inline writeCurrentBlock () =
#endif
        if instructions.Count > 0 then
            blocks.Add
                { CodeBlock.ExceptionHandler = ValueNone
                  CodeBlock.Locals = blockLocalMap.ToImmutable()
                  CodeBlock.Instructions = instructions.ToImmutable() }

        blockLocalMap.Clear()
        instructions.Clear()
        nextTemporaryIndex <- 0u

    let inline writeTypedExpression expression =
        writeExpressionCode
            createTemporaryRegister
            dataIndexLookup
            typeSignatureLookup
            importedMethodIndices
            definedMethodIndices
            localRegisterLookup
            instructions
            expression

    let rec writeBlockCode (method: CheckedMethod) (statements: ImmutableArray<_>) =
        for statement in statements do
            match statement with
            | CheckedStatement.Expression value ->
                writeTypedExpression value |> ignore
            | CheckedStatement.If(condition, thenBlockStatements, elseBlockStatements) ->
                let values = writeTypedExpression condition
                if values.Length <> 1 then
                    invalidOp "Expected only one condition expression for if statement"

                if not thenBlockStatements.IsDefaultOrEmpty then
                    instructions.Add(InstructionSet.Br_true(values.[0], 1, 2))
                    writeCurrentBlock() // End of if

                    writeBlockCode method thenBlockStatements // Start of then
                    if not((*previous instruction is a branch or ret*) false) then instructions.Add(InstructionSet.Br 1)
                    writeCurrentBlock() // End of then

                    writeBlockCode method elseBlockStatements // Start of else
                    if not((*previous instruction is a branch or ret*) false) then instructions.Add(InstructionSet.Br 1)
                    writeCurrentBlock() // End of else
                else
                    writeBlockCode method elseBlockStatements
            | CheckedStatement.LocalDeclaration(_, name, _, value) ->
                let values = writeTypedExpression value
                if values.Length <> 1 then
                    raise(NotImplementedException(sprintf "Code generation for local variable containing %i values is not yet implemented" values.Length))

                let (Index i) = values.[0]
                if i >= nextTemporaryIndex then
                    invalidOp "Expected temporary register but expression code returned a local or argument register"

                let tindex = TemporaryIndex.Index i
                let lindex = uint32 localRegisterLookup.Count

                blockLocalMap.Add(struct(tindex, LocalIndex.Index lindex))
                localRegisterLookup.Add(name.Content, fun() -> RegisterIndex.Index(nextTemporaryIndex + uint32 method.Parameters.Length + lindex))
            | CheckedStatement.Return values ->
                if values.IsDefaultOrEmpty then
                    instructions.Add(InstructionSet.Ret ImmutableArray.Empty)
                elif values.Length = 1 then
                    let mutable returns = Array.zeroCreate values.Length
                    for i = 0 to returns.Length - 1 do
                        let expressions = writeTypedExpression values.[i]
                        if expressions.Length <> 1 then
                            raise(NotImplementedException "TODO: Handle weird number of return values")
                        returns.[i] <- expressions.[0]
                    instructions.Add(InstructionSet.Ret(Unsafe.As<RegisterIndex[], ImmutableArray<RegisterIndex>> &returns))
                else
                    raise(NotImplementedException "Code generation for multiple return values is not yet implemented")
            | CheckedStatement.While(condition, body) ->
                writeCurrentBlock() // Start of condition

                let values = writeTypedExpression condition
                if values.Length <> 1 then
                    invalidOp "Expected only one condition expression for while loop"

                instructions.Add(InstructionSet.Br_true(values.[0], 1, 2))
                writeCurrentBlock() // End of condition

                writeBlockCode method body // Start of body
                instructions.Add(InstructionSet.Br -1)
                writeCurrentBlock() // End of body
            | CheckedStatement.Empty ->
                instructions.Add InstructionSet.Nop

    for KeyValue(method, struct(Index index, statements)) in methodBodyLookup do
        localRegisterLookup.Clear()
        blocks.Clear()

        for i = 0 to method.Parameters.Length - 1 do
            localRegisterLookup.Add(method.Parameters.[i].Name.Content, fun() -> RegisterIndex.Index(nextTemporaryIndex + uint32 i))

        writeBlockCode method statements

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

    let mutable mapElementType = Unchecked.defaultof<_>

    let mapReferenceType rtype =
        match rtype with
        | CheckedReferenceType.Any -> ReferenceType.Any
        | CheckedReferenceType.Array etype -> ReferenceType.Vector(mapElementType etype)

    mapElementType <-
        // TODO: Cache element types.
        function
        | CheckedElementType.ValueType(CheckedValueType.Primitive prim) ->
            ReferenceOrValueType.Value(ValueType.Primitive prim)
        | CheckedElementType.ReferenceType rtype ->
            ReferenceOrValueType.Reference(mapReferenceType rtype)

    let struct(tsignatures, typeSignatureLookup: CheckedType -> TypeSignatureIndex) =
        createIndexedLookup
            EqualityComparer.Default
            (function
            | CheckedType.ValueType(CheckedValueType.Primitive prim) ->
                AnyType.primitive prim
            | CheckedType.ReferenceType rtype ->
                AnyType.ReferenceType(mapReferenceType rtype)
            | CheckedType.Void ->
                invalidOp "Cannot create a type signature for void")

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

    let struct(importedTypeDefinitions, typeImportLookup) =
        writeTypeImports identifierIndexLookup moduleImportLookup mdl.ImportedTypes

    let translateExternalType (rm: UByte.Resolver.ResolvedModule) =
        function
        | AnyType.ValueType(ValueType.Primitive prim) -> CheckedType.primitive prim
        | AnyType.SafePointer _ -> raise(NotImplementedException "TODO: Add support for managed pointers")
        | bad -> failwithf "TODO: Implement translation of %A" bad

    let struct(importedMethodDefinitions, methodImportLookup) =
        writeMethodImports identifierIndexLookup methodSignatureLookup typeImportLookup translateExternalType mdl.ImportedMethods

    let definedTypeIndices = assignDefinedTypeIndices importedTypeDefinitions.Length mdl.DefinedTypes

    let struct(declaredMethodDefinitions, methodDefinitionLookup, methodBodyLookup) =
        writeMethodDefinitions
            identifierIndexLookup
            methodSignatureLookup
            definedTypeIndices
            importedMethodDefinitions.Length
            mdl.DefinedMethods

    let declaredTypeDefinitions =
        writeTypeDefinitions
            identifierIndexLookup
            namespaceIndexLookup
            typeImportLookup
            methodDefinitionLookup
            definedTypeIndices

    let code = writeMethodBodies dataIndexLookup typeSignatureLookup methodImportLookup methodDefinitionLookup methodBodyLookup

    //writeDebugInformation

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
          ModuleImports.ImportedTypes = importedTypeDefinitions
          ModuleImports.ImportedFields = ImmutableArray.Empty
          ModuleImports.ImportedMethods = importedMethodDefinitions }
      Module.Definitions =
        { ModuleDefinitions.DefinedTypes = declaredTypeDefinitions
          ModuleDefinitions.DefinedFields = ImmutableArray.Empty
          ModuleDefinitions.DefinedMethods = declaredMethodDefinitions }
      Module.Data = data.ToImmutable()
      Module.Code = code
      Module.EntryPoint =
        match mdl.EntryPoint with
        | ValueSome main -> ValueSome methodDefinitionLookup.[main]
        | ValueNone -> ValueNone
      Module.Debug = () }
