module UByte.Format.Model

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text

[<RequireQualifiedAccess; Struct>]
type Magic = internal { Magic: ImmutableArray<byte> }

let (|Magic|) { Magic.Magic = magic } = magic

let mutable magic' = "ubyte"B

let magic = { Magic.Magic = Unsafe.As &magic' }

type vector<'T> = ImmutableArray<'T>

type uvarint = uint32

type varint = int32

[<Struct; CustomComparison; CustomEquality>]
type VersionNumbers =
    | VersionNumbers of vector<uint32>

    override this.ToString() =
        let (VersionNumbers numbers) = this
        if numbers.IsDefaultOrEmpty
        then "0"
        else
            let sb = StringBuilder(2 * numbers.Length - 1)
            for i = 0 to numbers.Length - 1 do
                if i > 0 then sb.Append '.' |> ignore
                sb.Append numbers.[i] |> ignore
            sb.ToString()

    override this.GetHashCode() =
        let (VersionNumbers numbers) = this
        let mutable hcode = 0
        for i = 0 to numbers.Length - 1 do hcode <- hcode + int32 numbers.[i]
        hcode

    // TODO: If length differs, treat missing numbers as zeroes
    interface IEquatable<VersionNumbers> with
        member this.Equals(VersionNumbers other) =
            let (VersionNumbers this') = this
            Equality.lists this' other

    override this.Equals o =
        match o with
        | :? VersionNumbers as other -> this === other
        | _ -> false

    // TODO: If length differs, treat missing numbers as zeroes
    interface IComparable<VersionNumbers> with
        member this.CompareTo(VersionNumbers other) =
            let (VersionNumbers this') = this
            Comparison.lists this' other

    interface IComparable with member this.CompareTo o = Comparison.compare this (o :?> VersionNumbers)

let [<Literal>] MaxDataVectorCount : uvarint = 11u

let currentFormatVersion = ImmutableArray.Create(0u, 0u) |> VersionNumbers
let currentDataVectorCount = MaxDataVectorCount

type ustring = string

[<Struct; NoComparison; StructuralEquality>]
type Name =
    | Name of ustring

    override this.ToString() = let (Name name) = this in name

type LengthEncoded<'Contents> = 'Contents

module IndexKinds =
    type [<AbstractClass>] Kind = class end
    type [<Sealed; Class>] Identifier = inherit Kind
    type [<Sealed; Class>] Namespace = inherit Kind
    type [<Sealed; Class>] TypeSignature = inherit Kind
    type [<Sealed; Class>] MethodSignature = inherit Kind
    type [<Sealed; Class>] Module = inherit Kind
    type [<Sealed; Class>] TypeDefinition = inherit Kind
    type [<Sealed; Class>] Method = inherit Kind
    type [<Sealed; Class>] Field = inherit Kind
    type [<Sealed; Class>] Data = inherit Kind
    type [<Sealed; Class>] Code = inherit Kind
    type [<Sealed; Class>] Block = inherit Kind
    type [<Sealed; Class>] TemporaryRegister = inherit Kind
    type [<Sealed; Class>] LocalRegister = inherit Kind
    type [<Sealed; Class>] Register = inherit Kind

[<Struct; StructuralComparison; StructuralEquality>]
type Index<'Kind when 'Kind :> IndexKinds.Kind> =
    | Index of uvarint

type IdentifierIndex = Index<IndexKinds.Identifier>
type NamespaceIndex = Index<IndexKinds.Namespace>
type TypeSignatureIndex = Index<IndexKinds.TypeSignature>
type MethodSignatureIndex = Index<IndexKinds.MethodSignature>
type ModuleIndex = Index<IndexKinds.Module>
type TypeDefinitionIndex = Index<IndexKinds.TypeDefinition>
type MethodIndex = Index<IndexKinds.Method>
type FieldIndex = Index<IndexKinds.Field>
type CodeIndex = Index<IndexKinds.Code>
type CodeBlockIndex = Index<IndexKinds.Block>
type RegisterIndex = Index<IndexKinds.Register>
type TemporaryIndex = Index<IndexKinds.TemporaryRegister>
type LocalIndex = Index<IndexKinds.LocalRegister>
type DataIndex = Index<IndexKinds.Data>

[<RequireQualifiedAccess>]
type PrimitiveType =
    | Bool
    | U8
    | S8
    | U16
    | S16
    | U32
    | S32
    | U64
    | S64
    | UNative
    | SNative
    | Char16
    | Char32
    | F32
    | F64
    | Unit

module InstructionSet =
    type BlockOffset = varint

    type Opcode =
        | nop = 0u
        | ret = 1u
        | phi = 2u
        | ``select`` = 3u
        | call = 4u
        | ``call.virt`` = 5u
        | ``call.indr`` = 6u
        | ``global.ld`` = 0x1Du
        | ``global.st`` = 0x1Eu
        | ``global.addr`` = 0x1Fu
        | add = 0x20u
        | sub = 0x21u
        | mul = 0x22u
        | div = 0x23u
        | incr = 0x24u
        | decr = 0x25u
        | ``and`` = 0x26u
        | ``or`` = 0x27u
        | ``not`` = 0x28u
        | ``xor`` = 0x29u
        | rem = 0x30u
        | rotl = 0x34u
        | rotr = 0x35u
        | ``const.s`` = 0x40u
        | ``const.u`` = 0x41u
        | ``const.f32`` = 0x42u
        | ``const.f64`` = 0x43u
        | ``const.true`` = 0x44u
        | ``const.zero`` = 0x45u
        | ``const.false`` = 0x45u
        | br = 0x50u
        | ``br.eq`` = 0x51u
        | ``br.ne`` = 0x52u
        | ``br.lt`` = 0x53u
        | ``br.gt`` = 0x54u
        | ``br.le`` = 0x55u
        | ``br.ge`` = 0x56u
        | ``br.true`` = 0x57u
        | ``mem.init`` = 0x6Au
        | ``mem.st`` = 0x6Bu
        | ``mem.ld`` = 0x6Cu
        | ``mem.cpy`` = 0x6Du
        | ``mem.init.const`` = 0x6Eu
        | ``obj.new`` = 0x70u
        | ``obj.null`` = 0x71u
        | ``obj.fd.ld`` = 0x72u
        | ``obj.fd.st`` = 0x73u
        | ``obj.fd.addr`` = 0x74u
        | ``obj.throw`` = 0x75u
        | ``obj.arr.new`` = 0x7Au
        | ``obj.arr.len`` = 0x7Bu
        | ``obj.arr.get`` = 0x7Cu
        | ``obj.arr.set`` = 0x7Eu
        | ``obj.arr.addr`` = 0x7Du
        | ``obj.arr.const`` = 0x7Fu
        | alloca = 0xAAu
        | ``alloca.obj`` = 0xABu

    [<Flags>]
    type ArithmeticFlags =
        | None = 0uy
        | ThrowOnOverflow = 1uy
        | ThrowOnDivideByZero = 1uy
        | ValidMask = 1uy

    [<Flags>]
    type CallFlags =
        | None = 0uy
        | NoTailCallOptimization = 1uy
        | RequiresTailCallOptimization = 0b0000_0010uy
        | ThrowOnNullThis = 0b1000_0000uy

    [<Flags>]
    type AllocationFlags =
        | None = 0uy
        | ThrowOnFailure = 1uy

    [<Flags>]
    type MemoryAccessFlags =
        | None = 0uy
        | ThrowOnInvalidAccess = 1uy
        /// <summary>Applicable to <c>mem.ld</c>, <c>mem.st</c>, <c>mem.init</c>, and <c>mem.init.const</c> only.</summary>
        | AllowUnaligned = 2uy

    type Instruction =
        | Nop
        | Ret of results: vector<RegisterIndex>
        | Phi of vector<struct(RegisterIndex * BlockOffset)>
        | Select of condition: RegisterIndex * vtrue: RegisterIndex * vfalse: RegisterIndex
        | Call of CallFlags * method: MethodIndex * arguments: vector<RegisterIndex>
        | Call_virt of CallFlags * method: MethodIndex * this: RegisterIndex * arguments: vector<RegisterIndex>
        | Add of ArithmeticFlags * PrimitiveType * x: RegisterIndex * y: RegisterIndex
        | Sub of ArithmeticFlags * PrimitiveType * x: RegisterIndex * y: RegisterIndex
        | Mul of ArithmeticFlags * PrimitiveType * x: RegisterIndex * y: RegisterIndex
        | Div of ArithmeticFlags * PrimitiveType * x: RegisterIndex * y: RegisterIndex
        | Incr of ArithmeticFlags * PrimitiveType * RegisterIndex
        | Decr of ArithmeticFlags * PrimitiveType * RegisterIndex
        | And of PrimitiveType * x: RegisterIndex * y: RegisterIndex
        | Or of PrimitiveType * x: RegisterIndex * y: RegisterIndex
        | Not of PrimitiveType * RegisterIndex
        | Xor of PrimitiveType * x: RegisterIndex * y: RegisterIndex
        | Rem of ArithmeticFlags * PrimitiveType * x: RegisterIndex * y: RegisterIndex
        | Rotl of PrimitiveType * amount: RegisterIndex * i: RegisterIndex
        | Rotr of PrimitiveType * amount: RegisterIndex * i: RegisterIndex
        | Const_s of PrimitiveType * value: varint
        | Const_u of PrimitiveType * value: uvarint
        | Const_f32 of value: single
        | Const_f64 of value: double
        | Const_true of PrimitiveType
        | Const_false of PrimitiveType
        | Const_zero of PrimitiveType
        | Br of target: BlockOffset
        | Br_eq of x: RegisterIndex * y: RegisterIndex * btrue: BlockOffset * bfalse: BlockOffset
        | Br_ne of x: RegisterIndex * y: RegisterIndex * btrue: BlockOffset * bfalse: BlockOffset
        | Br_lt of x: RegisterIndex * y: RegisterIndex * btrue: BlockOffset * bfalse: BlockOffset
        | Br_gt of x: RegisterIndex * y: RegisterIndex * btrue: BlockOffset * bfalse: BlockOffset
        | Br_le of x: RegisterIndex * y: RegisterIndex * btrue: BlockOffset * bfalse: BlockOffset
        | Br_ge of x: RegisterIndex * y: RegisterIndex * btrue: BlockOffset * bfalse: BlockOffset
        | Br_true of condition: RegisterIndex * btrue: BlockOffset * bfalse: BlockOffset
        | Mem_init of MemoryAccessFlags * count: RegisterIndex * TypeSignatureIndex * address: RegisterIndex * value: RegisterIndex
        | Mem_st of MemoryAccessFlags * address: RegisterIndex * TypeSignatureIndex * value: RegisterIndex
        | Mem_cpy of MemoryAccessFlags * count: RegisterIndex * TypeSignatureIndex * source: RegisterIndex * destination: RegisterIndex
        | Mem_ld of MemoryAccessFlags * TypeSignatureIndex * address: RegisterIndex
        | Mem_init_const of MemoryAccessFlags * TypeSignatureIndex * address: RegisterIndex * data: DataIndex
        | Obj_new of constructor: MethodIndex * arguments: vector<RegisterIndex>
        | Obj_null
        | Obj_fd_ld of field: FieldIndex * object: RegisterIndex
        | Obj_fd_st of field: FieldIndex * object: RegisterIndex * source: RegisterIndex
        | Obj_fd_addr of MemoryAccessFlags * field: FieldIndex * object: RegisterIndex
        | Obj_throw of ex: RegisterIndex
        | Obj_arr_new of etype: TypeSignatureIndex * length: RegisterIndex
        | Obj_arr_len of ArithmeticFlags * PrimitiveType * array: RegisterIndex
        | Obj_arr_get of array: RegisterIndex * index: RegisterIndex
        | Obj_arr_set of array: RegisterIndex * index: RegisterIndex * source: RegisterIndex
        | Obj_arr_addr of MemoryAccessFlags * array: RegisterIndex * index: RegisterIndex
        | Obj_arr_const of etype: TypeSignatureIndex * data: DataIndex
        | Alloca of AllocationFlags * count: RegisterIndex * TypeSignatureIndex
        | Alloca_obj of AllocationFlags * constructor: MethodIndex * arguments: vector<RegisterIndex>

[<RequireQualifiedAccess>]
type IdentifierSection =
    { Identifiers: vector<string> }

    member this.Item with get (Index i: IdentifierIndex) = this.Identifiers.[Checked.int32 i]

module Tag =
    type MethodBody =
        | Defined = 0uy
        | Abstract = 1uy
        | External = 2uy

    type TypeDefinitionLayout =
        | Unspecified = 0uy
        | Sequential = 1uy

    type Type =
        | Unit = 0uy
        | S8 = 1uy
        | S16 = 2uy
        | S32 = 4uy
        | S64 = 8uy
        | RefVector = 0xAuy
        | U8 = 0x10uy
        | U16 = 0x20uy
        | U32 = 0x40uy
        | SNative = 0x49uy
        | UNative = 0x55uy
        | U64 = 0x80uy
        | DefinedStruct = 0xA1uy
        | RefAny = 0xAAuy
        | Bool = 0xB0uy
        | RefBoxed = 0xBBuy
        | Char16 = 0xC2uy
        | Char32 = 0xC4uy
        | UnsafePointer = 0xCCuy
        | SafePointer = 0xCEuy
        | ObjectPointer = 0xCFuy
        | RefDefinedType = 0xDEuy
        | F32 = 0xF4uy
        | F64 = 0xF8uy

[<RequireQualifiedAccess>]
type ValueType =
    | Primitive of PrimitiveType
    | Defined of TypeDefinitionIndex
    | UnsafePointer of ValueType

[<RequireQualifiedAccess>]
type ReferenceType =
    | Defined of TypeDefinitionIndex
    | BoxedValueType of ValueType
    | Any
    | Vector of ReferenceOrValueType

and [<RequireQualifiedAccess>] ReferenceOrValueType =
    | Reference of ReferenceType
    | Value of ValueType

type AnyType =
    | ValueType of ValueType
    | ReferenceType of ReferenceType
    | SafePointer of ReferenceOrValueType

type MethodSignature = { ReturnTypes: vector<TypeSignatureIndex>; ParameterTypes: vector<TypeSignatureIndex> }

type FieldImport = { FieldOwner: TypeDefinitionIndex; FieldName: IdentifierIndex; FieldType: TypeSignatureIndex }

type MethodImport =
    { MethodOwner: TypeDefinitionIndex
      MethodName: IdentifierIndex
      TypeParameters: uvarint
      Signature: MethodSignatureIndex }

type TypeDefinitionImport =
    { Module: ModuleIndex
      TypeName: IdentifierIndex
      TypeNamespace: NamespaceIndex
      TypeParameters: uvarint }

type VisibilityFlags =
    | Unspecified = 0uy
    | Public = 1uy
    | Private = 2uy

[<Flags>]
type FieldFlags =
    /// Indicates that the field can only be mutated in a class constructor or in a type initializer.
    | ReadOnly = 0uy
    | Mutable = 0b0000_0001uy
    /// Indicates whether the field is a global, which means that the field does not belong to instances of the type.
    | Static = 0b0000_0010uy
    | ValidMask = 0b0000_0011uy

type Field =
    { FieldOwner: TypeDefinitionIndex
      FieldName: IdentifierIndex
      FieldVisibility: VisibilityFlags
      FieldFlags: FieldFlags
      FieldType: TypeSignatureIndex
      FieldAnnotations: vector<unit> }

[<Flags>]
type MethodFlags =
    | Final = 0uy
    | Instance = 0b0000_0001uy
    | Constructor = 0b0000_0010uy
    | ConstructorMask = 0b0000_0011uy
    | Virtual = 0b0000_0100uy
    | ValidMask = 0b0000_0111uy

[<RequireQualifiedAccess>]
type MethodBody =
    | Defined of CodeIndex
    | Abstract
    | External of library: IdentifierIndex * entryPointFunction: IdentifierIndex

type Method =
    { MethodOwner: TypeDefinitionIndex
      MethodName: IdentifierIndex
      MethodVisibility: VisibilityFlags
      MethodFlags: MethodFlags
      TypeParameters: vector<unit>
      Signature: MethodSignatureIndex
      MethodAnnotations: vector<unit>
      Body: MethodBody }

type TypeAlias =
    { AliasName: IdentifierIndex
      AliasVisibility: VisibilityFlags
      AliasOf: TypeSignatureIndex }

[<Flags>]
type TypeDefinitionFlags =
    | Final = 0uy
    | NotFinal = 0b0000_0001uy
    | Abstract = 0b0000_0010uy
    | ReferenceOnly = 0b0000_0100uy
    | StackOnly = 0b0000_1000uy
    | StorageKindMask = 0b0000_1100uy
    | ValidMask = 0b0000_1111uy

type TypeDefinitionLayout =
    | Unspecified
    | Sequential

[<Struct>]
type MethodOverride = { Declaration: MethodIndex; Implementation: MethodIndex }

type TypeDefinition =
    { TypeName: IdentifierIndex
      TypeNamespace: NamespaceIndex
      TypeVisibility: VisibilityFlags
      TypeFlags: TypeDefinitionFlags
      TypeLayout: TypeDefinitionLayout
      TypeParameters: vector<unit>
      InheritedTypes: vector<TypeDefinitionIndex>
      TypeAnnotations: vector<unit>
      Fields: vector<FieldIndex>
      Methods: vector<MethodIndex>
      VTable: vector<MethodOverride> }

[<Flags>]
type RegisterFlags =
    | None = 0uy
    | ValidMask = 0b0000_0000uy

[<Struct>]
type RegisterType = { RegisterType: TypeSignatureIndex; RegisterFlags: RegisterFlags }

type BlockExceptionHandler =
    { ExceptionRegister: LocalIndex voption
      CatchBlock: CodeBlockIndex }

type CodeBlockFlags =
    | None = 0uy
    | ExceptionHandlerIgnoresException = 1uy
    | ExceptionHandlerStoresException = 2uy
    | ExceptionHandlingMask = 2uy

type CodeBlock =
    { ExceptionHandler: BlockExceptionHandler voption
      Locals: vector<struct(TemporaryIndex * LocalIndex)>
      Instructions: LengthEncoded<vector<InstructionSet.Instruction>> }

    member this.Flags =
        match this.ExceptionHandler with
        | ValueSome { ExceptionRegister = ValueSome _ } -> CodeBlockFlags.ExceptionHandlerStoresException
        | ValueSome { ExceptionRegister = ValueNone } -> CodeBlockFlags.ExceptionHandlerIgnoresException
        | ValueNone -> CodeBlockFlags.None

type Code = { LocalCount: uvarint; Blocks: vector<CodeBlock> }

type Debug = unit

[<NoComparison; StructuralEquality>]
type ModuleIdentifier = { ModuleName: Name; Version: VersionNumbers }

[<Flags>]
type ModuleHeaderFlags =
    | LittleEndian = 0uy
    | BigEndian = 0b0000_0001uy
    | NoGarbageCollector = 0b0000_0010uy
    | ValidMask = 0b0000_0001uy

type PointerSize =
    | Unspecified = 0uy
    | Is32Bit = 1uy
    | Is64Bit = 2uy

[<Struct>]
type Endianness = | LittleEndian | BigEndian

type ModuleHeader =
    { Module: ModuleIdentifier
      Flags: ModuleHeaderFlags
      PointerSize: PointerSize }

    member _.FieldCount = 3u

    member this.Endianness =
        if this.Flags &&& ModuleHeaderFlags.BigEndian <> ModuleHeaderFlags.LittleEndian
        then BigEndian
        else LittleEndian

type ModuleImports =
    { ImportedModules: vector<ModuleIdentifier>
      ImportedTypes: LengthEncoded<vector<TypeDefinitionImport>>
      ImportedFields: LengthEncoded<vector<FieldImport>>
      ImportedMethods: LengthEncoded<vector<MethodImport>> }

type ModuleDefinitions =
    { DefinedTypes: LengthEncoded<vector<TypeDefinition>>
      DefinedFields: LengthEncoded<vector<Field>>
      DefinedMethods: LengthEncoded<vector<Method>> }

type Module =
    { Magic: Magic
      FormatVersion: VersionNumbers
      Header: LengthEncoded<ModuleHeader>
      Identifiers: LengthEncoded<IdentifierSection>
      Namespaces: LengthEncoded<vector<vector<IdentifierIndex>>>
      TypeSignatures: LengthEncoded<vector<AnyType>>
      MethodSignatures: LengthEncoded<vector<MethodSignature>>
      Imports: LengthEncoded<ModuleImports>
      Definitions: LengthEncoded<ModuleDefinitions>
      Data: LengthEncoded<vector<vector<byte>>>
      Code: LengthEncoded<vector<Code>>
      EntryPoint: LengthEncoded<MethodIndex voption>
      Debug: LengthEncoded<Debug> }

    member this.Endianness = this.Header.Endianness

    member _.DataVectorCount: uvarint = currentDataVectorCount

let (|Name|) (Name name) = name

module Name =
    let tryOfStr name =
        if String.IsNullOrEmpty name
        then ValueNone
        else ValueSome(Name name)

    let ofStr name =
        match tryOfStr name with
        | ValueSome name' -> name'
        | ValueNone -> invalidArg (nameof name) "The name must not be empty"

module MethodSignature =
    let empty =
        { ReturnTypes = ImmutableArray.Empty
          ParameterTypes = ImmutableArray.Empty }

module VersionNumbers =
    let empty = VersionNumbers ImmutableArray.Empty
    let semver major minor patch = VersionNumbers(ImmutableArray.Create(major, minor, item3 = patch))
    let ofValueOption version = ValueOption.defaultValue empty version
    let ofList version =
        match version with
        | [] -> empty
        | _ -> VersionNumbers(ImmutableArray.CreateRange version)
