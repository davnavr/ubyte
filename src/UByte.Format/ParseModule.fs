module UByte.Format.ParseModule

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.CompilerServices

open UByte.Format.Model
open UByte.Format.Model.InstructionSet

[<Interface>]
type IByteSequence = abstract Read: buffer: Span<byte> -> int32

let u1 (stream: #IByteSequence) =
    let buffer = Span.stackalloc<byte> 1
    if stream.Read buffer = 1
    then buffer.[0]
    else failwith "TODO: EOF unexpectedly reached"

let inline bits1<'Enum, 'Source when 'Enum : enum<uint8> and 'Source :> IByteSequence> (stream: 'Source) =
    LanguagePrimitives.EnumOfValue<_, 'Enum>(u1 stream)

[<Interface>]
type IParser<'Result> = abstract Parse: source: #IByteSequence -> 'Result

let inline parse<'Parser, 'Result, 'Source
    when 'Parser :> IParser<'Result>
    and 'Parser : struct
    and 'Source :> IByteSequence>
    (source: 'Source)
    =
    Unchecked.defaultof<'Parser>.Parse source

[<RequireQualifiedAccess>]
module LEB128 =
    let [<Literal>] private ContinueMask = 0b1000_0000uy

    let inline private unsigned size convert (stream: #IByteSequence) =
        let mutable cont, n, shifted = true, LanguagePrimitives.GenericZero, 0
        while cont do
            let b = u1 stream
            cont <- b &&& ContinueMask = ContinueMask
            n <- n ||| (convert (b &&& (~~~ContinueMask)) <<< shifted)

            let shifted' =
                if cont then 7
                elif b >= 64uy then 6
                elif b >= 32uy then 5
                elif b >= 16uy then 4
                elif b >= 8uy then 3
                elif b >= 4uy then 2
                elif b >= 2uy then 1
                else 0

            shifted <- Checked.(+) shifted shifted'
            if shifted > size then failwith "TODO: Error for exceeded max allowed value for this kind of LEB128 integer"
        n

    [<Struct; NoComparison; NoEquality>]
    type UInt =
        interface IParser<uvarint> with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member _.Parse source = unsigned 32 uint32 source

[<IsReadOnly; Struct>]
type StreamWrapper (stream: Stream) =
    interface IByteSequence with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.Read buffer = stream.Read buffer

// TODO: Have base class for exceptions.
let magic (source: #IByteSequence) =
    let magic' = Model.magic.Magic
    let buffer = Span.stackalloc magic'.Length
    if source.Read buffer = 4 && Equality.spans (Span.asReadOnly buffer) (magic'.AsSpan()) then
        Model.magic
    else failwithf "TODO: Error for invalid magic"

let vector<'Parser, 'Result, 'Source
    when 'Parser :> IParser<'Result>
    and 'Parser : struct
    and 'Source :> IByteSequence>
    (source: 'Source)
    =
    let mutable items =
        let length = Checked.int32(parse<LEB128.UInt, _, _> source)
        Array.zeroCreate<'Result> length

    for i = 0 to items.Length - 1 do
        items.[i] <- parse<'Parser, 'Result, _> source

    Unsafe.As<_, ImmutableArray<'Result>> &items

let inline versions (source: #_) = VersionNumbers(vector<LEB128.UInt, _, _> source)

let inline index (source: #_) = Index(parse<LEB128.UInt, _, _> source)

[<Struct>]
type Index'<'Kind when 'Kind :> IndexKinds.Kind> =
    interface IParser<Index<'Kind>> with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.Parse source = index source

let lengthEncodedData (source: #IByteSequence) =
    let mutable data =
        parse<LEB128.UInt, _, _> source
        |> Checked.int32
        |> Array.zeroCreate<byte>
    let read = source.Read(Span(data, 0, data.Length))
    if read <> data.Length then failwithf "TODO: Unexpected end of data of length %i" data.Length
    struct(Unsafe.As<_, ImmutableArray<byte>> &data, StreamWrapper(new MemoryStream(data)))

let ustring (source: #_) =
    let struct(bytes, _) = lengthEncodedData source
    System.Text.Encoding.UTF8.GetString(bytes.AsSpan())

let name (source: #_) =
    match Name.tryOfStr(ustring source) with
    | ValueSome n -> n
    | ValueNone -> failwithf "TODO: Error for name cannot be empty"

let moduleID (source: #_) =
    { ModuleIdentifier.ModuleName = name source
      Version = versions source }

let header (source: #_) =
    let struct(_, header') = lengthEncodedData source
    let fcount = parse<LEB128.UInt, _, _> header'
    if fcount <> 3u then failwithf "TODO: Invalid field count %i" fcount

    { Module = moduleID header'
      Flags = bits1 header'
      PointerSize =
        let psize = bits1 header'
        if psize > PointerSize.Is64Bit then failwithf "TODO: Invalid pointer size 0x%02X (%A)" (uint8 psize) psize
        psize }

[<Struct>]
type Placeholder'<'T> =
    interface IParser<'T> with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.Parse _ = raise(NotImplementedException())

[<Struct>]
type Name' =
    interface IParser<Name> with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.Parse source = name source

[<Struct>]
type FieldImport' =
    interface IParser<FieldImport> with
        member _.Parse source =
            { FieldImport.FieldName = index source
              FieldType = index source }

[<Struct>]
type MethodImport' =
    interface IParser<MethodImport> with
        member _.Parse source =
            { MethodImport.MethodName = index source
              TypeParameters = parse<LEB128.UInt, _, _> source
              Signature = index source }

[<Struct>]
type TypeDefinitionImport' =
    interface IParser<TypeDefinitionImport> with
        member _.Parse source =
            { TypeDefinitionImport.TypeName = index source
              TypeKind =
                let kind = bits1 source
                if kind > Tag.TypeDefinitionKind.Struct then failwithf "TODO: Invalid type kind 0x%02X (%A)" (uint8 kind) kind
                kind
              TypeParameters = parse<LEB128.UInt, _, _> source
              Fields = vector<FieldImport', _, _> source
              Methods = vector<MethodImport', _, _> source }

[<Struct>]
type NamespaceImport' =
    interface IParser<NamespaceImport> with
        member _.Parse source =
            { NamespaceImport.NamespaceName = vector<Index'<_>, _, _> source
              TypeImports = vector<TypeDefinitionImport', _, _> source
              TypeAliases = vector<Index'<_>, _, _> source }

[<Struct>]
type ModuleImport' =
    interface IParser<ModuleImport> with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.Parse source =
            { ModuleImport.ImportedModule = moduleID source
              ImportedNamespaces = vector<NamespaceImport', _, _> source }

[<Struct>]
type TypeSignature =
    interface IParser<AnyType> with
        member _.Parse source =
            
            failwith "TODO: Check tag"

[<Struct>]
type MethodSignature' =
    interface IParser<MethodSignature> with
        member _.Parse source =
            { MethodSignature.ParameterTypes = vector<Index'<_>, _, _> source
              ReturnTypes = vector<Index'<_>, _, _> source }

[<Struct>]
type DataVector =
    interface IParser<vector<byte>> with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.Parse source =
            let struct(data, _) = lengthEncodedData source // NOTE: Data is duplicated twice.
            data

[<Struct>]
type Field' =
    interface IParser<Field> with
        member _.Parse source =
            { Field.FieldName = index source
              FieldVisibility = bits1 source
              FieldFlags = bits1 source
              FieldType = index source
              FieldAnnotations = vector<Placeholder'<_>, _, _> source }

[<Struct>]
type Method' =
    interface IParser<Method> with
        member _.Parse source =
            { Method.MethodName = index source
              MethodVisibility = bits1 source
              MethodFlags = bits1 source
              TypeParameters = vector<Placeholder'<_>, _, _> source
              Signature = index source
              MethodAnnotations = vector<Placeholder'<_>, _, _> source
              Body =
                match bits1 source with
                | MethodBodyTag.Defined -> MethodBody.Defined(index source)
                | MethodBodyTag.Abstract -> MethodBody.Abstract
                | bad -> failwithf "TODO: Bad method body kind 0x%02X" (uint8 bad) }

[<Struct>]
type TypeDefinition' =
    interface IParser<TypeDefinition> with
        member _.Parse source =
            { TypeDefinition.TypeName = index source
              TypeVisibility = bits1 source
              TypeKind =
                match bits1 source with
                | Tag.TypeDefinitionKind.Class -> Class(index source, bits1 source)
                | Tag.TypeDefinitionKind.Interface -> Interface
                | Tag.TypeDefinitionKind.Struct -> Struct
                | bad -> failwithf "TODO: Bad type definition kind 0x%02X" (uint8 bad)
              TypeLayout =
                match bits1 source with
                | Tag.TypeDefinitionLayout.Unspecified -> TypeDefinitionLayout.Unspecified
                | Tag.TypeDefinitionLayout.Sequential -> TypeDefinitionLayout.Sequential
                | bad -> failwithf "TODO: Bad type definition layout kind 0x%02X" (uint8 bad)
              ImplementedInterfaces = vector<Placeholder'<_>, _, _> source
              TypeParameters = vector<Placeholder'<_>, _, _> source
              TypeAnnotations = vector<Placeholder'<_>, _, _> source
              Fields = vector<Field', _, _> source
              Methods = vector<Method', _, _> source }

[<Struct>]
type TypeAlias' =
    interface IParser<TypeAlias> with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.Parse source =
            { TypeAlias.AliasName = index source
              AliasVisibility = bits1 source
              AliasOf = failwith "TODO: Implement parsing of types" }

[<Struct>]
type Namespace' =
    interface IParser<Namespace> with
        member _.Parse source =
            { Namespace.NamespaceName = vector<Index'<_>, _, _> source
              TypeDefinitions =
                let struct(_, tdefs) = lengthEncodedData source
                vector<TypeDefinition', _, _> tdefs
              TypeAliases =
                let struct(_, taliases) = lengthEncodedData source
                vector<TypeAlias', _, _> taliases }

[<Struct>]
type CountedRegisterTypes =
    interface IParser<struct(uvarint * RegisterType)> with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.Parse source =
            let count = parse<LEB128.UInt, _, _> source
            let rtype =
                { RegisterType = index source
                  RegisterFlags = bits1 source }
            struct(count, rtype)

[<Struct>]
type InstructionDecoder =
    interface IParser<Instruction> with
        member _.Parse source =
            match LanguagePrimitives.EnumOfValue(parse<LEB128.UInt, _, _> source) with
            | Opcode.nop -> Nop

            // TODO: Maybe store number of return values in struction?
            //| Opcode.ret -> Ret(vector<Index'<_>, _, _> source)

            // TODO: Maybe store number of arguments in instruction?
            //| Opcode.call -> Call(index source, )

            // Register
            | Opcode.``reg.copy`` -> Reg_copy(index source, index source)

            // Arithmetic
            | Opcode.add -> Add(index source, index source, index source)
            | Opcode.sub -> Sub(index source, index source, index source)

            //| Opcode.``const.s32`` -> Const_s32 // TODO: Either store as LEB128, or somehow have access to module endianness

            | bad -> failwithf "TODO: Unrecognized opcode 0x08%X" (uint32 bad)

[<Struct>]
type Code' =
    interface IParser<Code> with
        member _.Parse source =
            { Code.RegisterTypes = vector<CountedRegisterTypes, _, _> source
              Instructions =
                let struct(_, instructions) = lengthEncodedData source
                vector<InstructionDecoder, _, _> source }

let fromBytes (source: #IByteSequence) =
    let magic' = magic source
    let fversion = versions source
    if fversion <> Model.currentFormatVersion then failwithf "TODO: Error for unsupported version %O" fversion
    let header' = header source

    // TODO: Check that length encoded data has no remaining bytes left
    { Module.Magic = magic'
      FormatVersion = fversion
      Header = header'
      Identifiers =
        let struct(_, identifiers) = lengthEncodedData source
        { IdentifierSection.Identifiers = vector<Name', _, _> identifiers }
      Imports =
        let struct(_, imports) = lengthEncodedData source
        vector<ModuleImport', _, _> imports
      TypeSignatures =
        let struct(_, tsigs) = lengthEncodedData source
        vector<TypeSignature, _, _> tsigs
      MethodSignatures =
        let struct(_, msigs) = lengthEncodedData source
        vector<MethodSignature', _, _> msigs
      Data =
        let struct(_, data) = lengthEncodedData source
        vector<DataVector, _, _> data
      Code =
        let struct(_, code) = lengthEncodedData source
        vector<Code', _, _> code
      Namespaces =
        let struct(_, namespaces) = lengthEncodedData source
        vector<Namespace', _, _> namespaces
      EntryPoint =
        let struct(epoint, epoint') = lengthEncodedData source
        if epoint.IsDefaultOrEmpty
        then ValueNone
        else ValueSome(index epoint')
      Debug =
        let struct(debug, _) = lengthEncodedData source
        if debug.Length > 0 then failwith "TODO: Debugging information not yet supported"
        () }

let fromStream (source: Stream) =
    if isNull source then nullArg(nameof source)
    try
        fromBytes(StreamWrapper source)
    finally
        source.Close()

let fromFile (source: FileInfo) =
    if isNull source then nullArg(nameof source)
    source.OpenWrite() |> fromStream

let fromPath source = fromFile(FileInfo source)
