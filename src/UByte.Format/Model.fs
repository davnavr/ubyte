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
type RegisterIndex = Index<IndexKinds.Register>
type DataIndex = Index<IndexKinds.Data>

module InstructionSet =
    type Opcode =
        | nop = 0u
        | ret = 1u
        | call = 5u
        | ``call.virt`` = 6u
        | ``reg.copy`` = 0x17u
        | add = 0x20u
        | sub = 0x21u
        | mul = 0x22u
        | div = 0x23u
        | ``add.ovf`` = 0x24u
        | ``sub.ovf`` = 0x25u
        | ``mul.ovf`` = 0x26u
        | incr = 0x2Cu
        | ``incr.ovf`` = 0x2Du
        | decr = 0x2Eu
        | ``decr.ovf`` = 0x2Fu
        | ``const.i32`` = 0x30u
        | ``const.i64`` = 0x31u
        | ``const.f32`` = 0x32u
        | ``const.f64`` = 0x33u
        | ``const.true`` = 0x34u
        | ``const.zero`` = 0x35u
        | ``const.false`` = 0x35u
        | ``and`` = 0x40u
        | ``or`` = 0x41u
        | ``not`` = 0x42u
        | ``xor`` = 0x43u
        | rem = 0x44u
        | rotl = 0x4Cu
        | rotr = 0x4Du
        | br = 0x60u
        | ``br.eq`` = 0x61u
        | ``obj.new`` = 0x70u
        | ``obj.null`` = 0x71u
        | ``obj.ldfd`` = 0x72u
        | ``obj.stfd`` = 0x73u
        | ``obj.throw`` = 0x74u
        | ``call.ret`` = 0x90u
        | ``call.virt.ret`` = 0x91u

    type Instruction =
        | Nop
        | Ret of results: vector<RegisterIndex>
        | Call of method: MethodIndex * arguments: ImmutableArray<RegisterIndex> * results: vector<RegisterIndex>
        | Call_virt of method: MethodIndex * arguments: vector<RegisterIndex> * results: vector<RegisterIndex>
        | Reg_copy of source: RegisterIndex * destination: RegisterIndex
        | Add of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        | Sub of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        | Mul of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        | Div of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        | Incr of RegisterIndex
        | Decr of RegisterIndex
        | Const_i32 of value: int32 * destination: RegisterIndex
        | Const_true of RegisterIndex
        | Const_false of RegisterIndex
        | Const_zero of RegisterIndex
        | And of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        | Or of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        | Not of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        | Xor of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        | Rem of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        | Rotl of amount: RegisterIndex * RegisterIndex
        | Rotr of amount: RegisterIndex * RegisterIndex
        | Obj_new of constructor: MethodIndex * arguments: vector<RegisterIndex> * result: RegisterIndex
        | Obj_null of destination: RegisterIndex
        | Obj_ldfd of field: FieldIndex * object: RegisterIndex * destination: RegisterIndex
        | Obj_stfd of field: FieldIndex * object: RegisterIndex * source: RegisterIndex
        | Call_ret of method: MethodIndex * arguments: vector<RegisterIndex> * results: vector<RegisterIndex>
        | Call_virt_ret of method: MethodIndex * arguments: vector<RegisterIndex> * results: vector<RegisterIndex>

[<RequireQualifiedAccess>]
type IdentifierSection =
    { Identifiers: vector<string> }

    member this.Item with get (Index i: IdentifierIndex) = this.Identifiers.[Checked.int32 i]

module Tag =
    type MethodBody =
        | Defined = 0uy
        | Abstract = 1uy

    type TypeDefinitionKind =
        /// Indicates that the class does not derive from another class.
        | BaseClass = 0uy
        | Class = 1uy
        | Interface = 2uy
        | Struct = 3uy

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
        | ValueType = 0xA1uy
        | RefAny = 0xAAuy
        | Bool = 0xB0uy
        | Char16 = 0xC2uy
        | Char32 = 0xC4uy
        | UnsafePointer = 0xCCuy
        | RefDefinedType = 0xDEuy
        | F32 = 0xF4uy
        | F64 = 0xF8uy

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
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

[<RequireQualifiedAccess>]
type ReferenceType =
    | Defined of TypeDefinitionIndex
    | Any

and [<NoComparison; StructuralEquality>]  AnyType =
    | Primitive of PrimitiveType
    | ObjectReference of ReferenceType
    | UnsafePointer of AnyType
    | ValueType of TypeDefinitionIndex

[<NoComparison; StructuralEquality>]
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
      TypeKind: Tag.TypeDefinitionKind
      TypeParameters: uvarint }

type VisibilityFlags =
    | Unspecified = 0uy
    | Public = 1uy
    | Private = 2uy

[<Flags>]
type FieldFlags =
    | ReadOnly = 0uy
    | Mutable = 0b0000_0001uy
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
    | ValidMask = 0b0000_0011uy

[<RequireQualifiedAccess>]
type MethodBody =
    | Defined of CodeIndex
    | Abstract

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
type ClassDefinitionFlags =
    | Final = 0uy
    | NotFinal = 0b0000_0001uy
    | Abstract = 0b0000_0010uy
    | ValidMask = 0b0000_0011uy

type TypeDefinitionKind =
    | Class of extends: TypeDefinitionIndex voption * flags: ClassDefinitionFlags
    | Interface
    | Struct

type TypeDefinitionLayout =
    | Unspecified
    | Sequential

type TypeDefinition =
    { TypeName: IdentifierIndex
      TypeNamespace: NamespaceIndex
      TypeVisibility: VisibilityFlags
      TypeKind: TypeDefinitionKind
      TypeLayout: TypeDefinitionLayout
      ImplementedInterfaces: vector<unit>
      TypeParameters: vector<unit>
      TypeAnnotations: vector<unit>
      Fields: vector<FieldIndex>
      Methods: vector<MethodIndex> }

[<Flags>]
type RegisterFlags =
    | None = 0uy
    | ValidMask = 0b0000_0000uy

[<Struct>]
type RegisterType = { RegisterType: TypeSignatureIndex; RegisterFlags: RegisterFlags }

type Code =
    { RegisterTypes: vector<struct(uvarint * RegisterType)>
      Instructions: LengthEncoded<vector<InstructionSet.Instruction>> }

    member this.RegisterCount =
        let mutable count = 0u
        for struct(count', _) in this.RegisterTypes do count <- count + count'
        count

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
