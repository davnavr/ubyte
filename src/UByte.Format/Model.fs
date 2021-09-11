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

let currentFormatVersion = ImmutableArray.Create(0u, 0u) |> VersionNumbers

type ustring = string

[<Struct; NoComparison; StructuralEquality>]
type Name =
    | Name of ustring

    override this.ToString() = let (Name name) = this in name

type LengthEncoded<'Contents> = 'Contents

module IndexKinds =
    type [<AbstractClass>] Kind = class end
    type [<Sealed; Class>] Identifier = inherit Kind
    type [<Sealed; Class>] TypeSignature = inherit Kind
    type [<Sealed; Class>] MethodSignature = inherit Kind
    type [<Sealed; Class>] Code = inherit Kind
    type [<Sealed; Class>] Register = inherit Kind
    type [<Sealed; Class>] Data = inherit Kind
    type [<Sealed; Class>] TypeDefinition = inherit Kind
    type [<Sealed; Class>] Method = inherit Kind

[<Struct; StructuralComparison; StructuralEquality>]
type Index<'Kind when 'Kind :> IndexKinds.Kind> =
    | Index of uvarint

type IdentifierIndex = Index<IndexKinds.Identifier>
type TypeSignatureIndex = Index<IndexKinds.TypeSignature>
type MethodSignatureIndex = Index<IndexKinds.MethodSignature>
type CodeIndex = Index<IndexKinds.Code>
type RegisterIndex = Index<IndexKinds.Register>
type DataIndex = Index<IndexKinds.Data>
type TypeDefinitionIndex = Index<IndexKinds.TypeDefinition>
type MethodIndex = Index<IndexKinds.Method>

module InstructionSet =
    type Opcode =
        | nop = 0u
        | ret = 1u
        | call = 0x10u
        | ``reg.copy`` = 0x17u
        | add = 0x20u
        | sub = 0x21u
        | mul = 0x22u
        | div = 0x23u
        | ``add.ovf`` = 0x24u
        | ``sub.ovf`` = 0x25u
        | ``mul.ovf`` = 0x26u
        | ``const.i32`` = 0x30u
        | ``const.i64`` = 0x31u
        | ``const.f32`` = 0x32u
        | ``const.f64`` = 0x33u
        | ``const.true`` = 0x34u
        | ``const.zero`` = 0x35u
        | ``const.false`` = 0x35u
        | br = 0x40u
        | ``br.eq`` = 0x41u

    type Instruction =
        | Nop
        | Ret of vector<RegisterIndex>
        | Call of method: MethodIndex * arguments: ImmutableArray<RegisterIndex> * results: vector<RegisterIndex>
        | Reg_copy of source: RegisterIndex * destination: RegisterIndex
        | Add of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        | Sub of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        | Const_i32 of value: int32 * destination: RegisterIndex

[<RequireQualifiedAccess>]
type IdentifierSection =
    { Identifiers: vector<Name> }

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
    | Char16
    | Char32
    | F32
    | F64
    | Unit

[<RequireQualifiedAccess>]
type ReferenceType =
    | Defined of TypeDefinitionIndex
    | Any of AnyType
    | Vector of AnyType

and [<NoComparison; StructuralEquality>]  AnyType =
    | Primitive of PrimitiveType
    | ObjectReference of ReferenceType
    | UnsafePointer of AnyType
    | ValueType of TypeDefinitionIndex

[<NoComparison; StructuralEquality>]
type MethodSignature = { ReturnTypes: vector<TypeSignatureIndex>; ParameterTypes: vector<TypeSignatureIndex> }

type FieldImport = { FieldName: IdentifierIndex; FieldType: TypeDefinitionIndex }

type MethodImport = { MethodName: IdentifierIndex; TypeParameters: uvarint; Signature: MethodSignatureIndex }

type TypeDefinitionImport =
    { TypeName: IdentifierIndex
      TypeKind: Tag.TypeDefinitionKind
      TypeParameters: uvarint
      Fields: vector<FieldImport>
      Methods: vector<MethodImport> }

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
    { FieldName: IdentifierIndex
      FieldVisibility: VisibilityFlags
      FieldFlags: FieldFlags
      FieldType: TypeSignatureIndex
      FieldAnnotations: vector<unit> }

[<Flags>]
type MethodFlags =
    | Final = 0uy
    | NotFinal = 0b0000_0001uy
    | Static = 0b0000_0010uy
    | ValidMask = 0b0000_0001uy

[<RequireQualifiedAccess>]
type MethodBody =
    | Defined of CodeIndex
    | Abstract

type Method =
    { MethodName: IdentifierIndex
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
      TypeVisibility: VisibilityFlags
      TypeKind: TypeDefinitionKind
      TypeLayout: TypeDefinitionLayout
      ImplementedInterfaces: vector<unit>
      TypeParameters: vector<unit>
      TypeAnnotations: vector<unit>
      Fields: vector<Field>
      Methods: vector<Method> }

type NamespaceImport =
    { NamespaceName: vector<IdentifierIndex>
      TypeImports: LengthEncoded<vector<TypeDefinitionImport>>
      TypeAliases: LengthEncoded<vector<IdentifierIndex>> }

type Namespace =
    { NamespaceName: vector<IdentifierIndex>
      TypeDefinitions: LengthEncoded<vector<TypeDefinition>>
      TypeAliases: LengthEncoded<vector<TypeAlias>> }

[<Flags>]
type RegisterFlags =
    | None = 0uy
    | Pinned = 0b0000_0010uy
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

[<NoComparison; StructuralEquality>]
type ModuleImport = { ImportedModule: ModuleIdentifier; ImportedNamespaces: vector<NamespaceImport> }

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

type ModuleHeader =
    { Module: ModuleIdentifier
      Flags: ModuleHeaderFlags
      PointerSize: PointerSize }

    member _.FieldCount = 3u

[<Struct>]
type Endianness = | LittleEndian | BigEndian

type Module =
    { Magic: Magic
      FormatVersion: VersionNumbers
      Header: LengthEncoded<ModuleHeader>
      Identifiers: LengthEncoded<IdentifierSection>
      Imports: LengthEncoded<vector<ModuleImport>>
      TypeSignatures: LengthEncoded<vector<AnyType>>
      MethodSignatures: LengthEncoded<vector<MethodSignature>>
      Data: LengthEncoded<vector<vector<byte>>>
      Code: LengthEncoded<vector<Code>>
      Namespaces: LengthEncoded<vector<Namespace>>
      EntryPoint: LengthEncoded<MethodIndex voption>
      Debug: LengthEncoded<Debug> }

    member this.Endianness =
        if this.Header.Flags &&& ModuleHeaderFlags.BigEndian <> ModuleHeaderFlags.LittleEndian
        then BigEndian
        else LittleEndian

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

module VersionNumbers =
    let semver major minor patch = VersionNumbers(ImmutableArray.Create(major, minor, item3 = patch))
