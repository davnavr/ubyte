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

type varint = uint32

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
    type [<Sealed; Class>] Data = inherit Kind
    type [<Sealed; Class>] Type = inherit Kind
    type [<Sealed; Class>] Method = inherit Kind
    type [<Sealed; Class>] Code = inherit Kind
    type [<Sealed; Class>] Register = inherit Kind

[<Struct; StructuralComparison; StructuralEquality>]
type Index<'Kind when 'Kind :> IndexKinds.Kind> =
    | Index of uvarint

type IdentifierIndex = Index<IndexKinds.Identifier>
type TypeIndex = Index<IndexKinds.Type>
type MethodIndex = Index<IndexKinds.Method>
type CodeIndex = Index<IndexKinds.Code>
type RegisterIndex = Index<IndexKinds.Register>

module InstructionSet =
    type Opcode =
        | nop = 0u
        | ret = 1u
        | call = 0x10u
        | ``reg.load`` = 0x15u
        | ``reg.store`` = 0x16u
        | ``reg.move`` = 0x17u
        | add = 0x20u
        | sub = 0x21u
        | mul = 0x22u
        | div = 0x23u
        | ``add.ovf`` = 0x24u
        | ``sub.ovf`` = 0x25u
        | ``mul.ovf`` = 0x26u
        | ``const.s32`` = 0x30u
        | ``const.s64`` = 0x31u
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
        | Add of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        | Const_s32 of value: int32 * destination: RegisterIndex

[<RequireQualifiedAccess>]
type IdentifierSection =
    { Identifiers: vector<Name> }

    member this.Item with get (Index i: IdentifierIndex) = this.Identifiers.[Checked.int32 i]

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
    | F32
    | F64
    | Unit

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type NonPrimitiveType =
    | FromModule of unit
    | Imported of unit

[<NoComparison; StructuralEquality>]
type AnyType =
    | PrimitiveType of PrimitiveType
    | ObjectReference of AnyType
    | UnsafePointer of AnyType
    | Vector of AnyType

[<NoComparison; StructuralEquality>]
type MethodSignature = { ReturnType: vector<TypeIndex>; ParameterTypes: vector<TypeIndex> }

type FieldImport = { FieldName: Name; FieldType: TypeIndex }

type MethodImport = { MethodName: Name; TypeParameters: uvarint; Signature: MethodSignature }

type TypeDefinitionKindTag =
    | Class = 0uy
    | Interface = 1uy
    | Struct = 2uy

type TypeDefinitionImport =
    { TypeName: Name
      TypeKind: TypeDefinitionKindTag
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
    { FieldName: Name
      FieldFlags: FieldFlags
      FieldVisibility: VisibilityFlags
      FieldType: TypeIndex
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
    { MethodName: Name
      MethodFlags: MethodFlags
      TypeParameters: vector<unit>
      MethodVisibility: VisibilityFlags
      Signature: MethodSignature
      MethodAnnotations: vector<unit>
      Body: MethodBody }

type TypeAlias =
    { AliasName: Name
      AliasVisibility: VisibilityFlags
      AliasOf: AnyType }

[<Flags>]
type ClassDefinitionFlags =
    | Final = 0uy
    | NotFinal = 0b0000_0001uy
    | Abstract = 0b0000_0010uy
    | ValidMask = 0b0000_0011uy

type TypeDefinitionKind =
    | Class of extends: TypeIndex * flags: ClassDefinitionFlags
    | Interface
    | Struct

    member this.Kind =
        match this with
        | Class(_, _) -> TypeDefinitionKindTag.Class
        | Interface -> TypeDefinitionKindTag.Interface
        | Struct -> TypeDefinitionKindTag.Struct

type TypeDefinitionLayout =
    | Unspecified
    | Sequential

type TypeDefinition =
    { TypeName: Name
      TypeKind: TypeDefinitionKind
      TypeVisibility: VisibilityFlags
      TypeLayout: TypeDefinitionLayout
      ImplementedInterfaces: vector<unit>
      TypeParameters: vector<unit>
      TypeAnnotations: vector<unit>
      Fields: vector<Field>
      Methods: vector<unit> }

type NamespaceImport =
    { NamespaceName: vector<IdentifierIndex>
      TypeImports: LengthEncoded<vector<TypeDefinitionImport>>
      TypeAliases: LengthEncoded<vector<Name>> }

type Namespace =
    { NamespaceName: vector<IdentifierIndex>
      TypeDefinitions: LengthEncoded<vector<TypeDefinition>>
      TypeAliases: LengthEncoded<vector<TypeAlias>> }

[<Flags>]
type RegisterFlags =
    | Pinned = 0b0000_0010uy
    | ValidMask = 0b0000_0000uy

[<Struct>]
type RegisterType = { RegisterType: TypeIndex; RegisterFlags: RegisterFlags }

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

type ModuleHeader =
    { Module: ModuleIdentifier
      Flags: ModuleHeaderFlags
      PointerSize: uvarint }

    member _.FieldCount = 3u

type Module =
    { Magic: Magic
      FormatVersion: VersionNumbers
      Header: LengthEncoded<ModuleHeader>
      Identifiers: LengthEncoded<IdentifierSection>
      Imports: LengthEncoded<vector<ModuleImport>>
      Data: LengthEncoded<vector<vector<byte>>>
      Namespaces: LengthEncoded<vector<Namespace>>
      EntryPoint: LengthEncoded<MethodIndex voption>
      Code: LengthEncoded<vector<Code>>
      Debug: LengthEncoded<Debug> }

let (|Name|) (Name name) = name

module Name =
    let tryOfStr name =
        if String.IsNullOrEmpty name
        then ValueNone
        else ValueSome(Name name)
