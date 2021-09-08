module UByte.Format.Format

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text

[<RequireQualifiedAccess; IsReadOnly; Struct>]
type Magic = internal { Magic: ImmutableArray<byte> }

val (|Magic|) : magic: Magic -> ImmutableArray<byte>

val magic : Magic

/// Represents an array preceded by an LEB128 encoded unsigned integer indicating the number of items.
type vector<'T> = ImmutableArray<'T>

/// Represents a LEB128 encoded unsigned integer.
type uvarint = uint32

/// Represents a LEB128 encoded signed integer.
type varint = uint32

/// A length-encoded array of LEB128 encoded unsigned integers used to indicate a version.
[<IsReadOnly; Struct; CustomComparison; CustomEquality>]
type VersionNumbers =
    | VersionNumbers of vector<uvarint>

    override ToString : unit -> string

    override GetHashCode : unit -> int32

    interface IEquatable<VersionNumbers>
    interface IComparable
    interface IComparable<VersionNumbers>

val currentFormatVersion : VersionNumbers

/// Represents a length-encoded UTF-8 string.
[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type UString =
    | UString of vector<Rune> // TODO: use byte instead as it will have smaller memory footprint

    override ToString : unit -> string

    interface IEquatable<UString>

/// Represents a length-encoded UTF-8 string that cannot be empty.
[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type Name =
    internal
    | Name of UString

    override ToString: unit -> string

    interface IEquatable<Name>

val (|Name|) : name: Name -> UString

/// Represents data that is preceded by a LEB128 unsigned integer indicating the byte length of the following data.
type LengthEncoded<'Data> = 'Data

[<RequireQualifiedAccess>]
module IndexKinds =
    type [<AbstractClass>] Kind = class end
    type [<Sealed; Class>] Identifier = inherit Kind
    type [<Sealed; Class>] Data = inherit Kind
    type [<Sealed; Class>] Type = inherit Kind
    type [<Sealed; Class>] MethodBody = inherit Kind
    type [<Sealed; Class>] Register = inherit Kind

[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type Index<'Kind when 'Kind :> IndexKinds.Kind> =
    | Index of uvarint

    interface IEquatable<Index<'Kind>>

/// <summary>Represents an index into the module's identifiers. The index of the first identifier is <c>0</c>.</summary>
type IdentifierIndex = Index<IndexKinds.Identifier>

/// <summary>Represents an index into the module's method bodies. The index of the first method body is <c>0</c>.</summary>
type MethodBodyIndex = Index<IndexKinds.MethodBody>

/// <summary>
/// Represents an index into the module's imported types or defined types. The index of the first imported type or type alias, if
/// any, is <c>0</c>. The index of the first type or type alias defined in this module is equal to the number of type aliases.
/// </summary>
type TypeIndex = Index<IndexKinds.Type>

/// <summary>
/// Represents an index to an index. The index of the first register not representing a method parameter is equal to the number
/// of method parameters. The index of the first method parameter, if any are defined, is <c>0</c>.
/// </summary>
type RegisterIndex = Index<IndexKinds.Register>

module InstructionSet =
    /// Opcodes are represented as LEB128 encoded unsigned integers.
    type Opcode =
        | nop = 0u
        | ret = 1u

        | call = 0x10u

        // Arithmetic
        | add = 0x20u
        | sub = 0x21u
        | mul = 0x22u
        | div = 0x23u

        | ``const.s32`` = 0x30u
        | ``const.s64`` = 0x31u
        | ``const.f32`` = 0x32u
        | ``const.f64`` = 0x33u
        | ``const.true`` = 0x34u
        | ``const.zero`` = 0x35u
        | ``const.false`` = 0x35u

        // Branching
        | br = 0x40u
        | ``br.eq`` = 0x41u

    [<NoComparison; NoEquality>]
    type Instruction =
        | Nop
        | Ret of vector<RegisterIndex>

        // Arithmetic
        /// <summary>
        /// Computes the sum of the values in registers <c>x</c> and <c>y</c>, and stores the sum in the <c>result</c> register.
        /// </summary>
        | Add of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex

        /// Stores a signed 32-bit integer into the specified register.
        | Const_s32 of int32 * RegisterIndex

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type IdentifierSection =
    { Identifiers: vector<Name> }

    member Item : index: IdentifierIndex -> Name

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
    | ObjectReference of AnyType // TODO: Allow read only pointers?
    | UnsafePointer of AnyType
    /// Represents a one-dimensional array whose first element is at index zero.
    | Vector of AnyType

    interface IEquatable<AnyType>

/// Describes the return types and parameter types of a method.
[<NoComparison; StructuralEquality>]
type MethodSignature =
    { /// The types of the values returned by the method. Currently, only one type is allowed.
      ReturnType: vector<TypeIndex>
      ParameterTypes: vector<TypeIndex> }

    interface IEquatable<MethodSignature>

[<NoComparison; NoEquality>]
type FieldImport =
    { FieldName: Name
      FieldType: TypeIndex }

[<NoComparison; NoEquality>]
type MethodImport =
    { MethodName: Name
      //TypeParameters: 
      Signature: MethodSignature }

type TypeDefinitionKindTag =
    | Class = 0uy
    | Interface = 1uy
    | Struct = 2uy

[<NoComparison; NoEquality>]
type TypeDefinitionImport =
    { TypeName: Name
      TypeKind: TypeDefinitionKindTag
      //TypeParameters: 
      Fields: vector<FieldImport>
      Methods: vector<MethodImport> }

/// Used to specify whether or not a type, field, or method can be imported by another module.
type VisibilityFlags =
    /// Compiler decides whether or not it can be used.
    | Unspecified = 0uy
    /// Can be imported by another module.
    | Public = 1uy
    /// Cannot be imported by another module, can only be used within the current module.
    | Private = 2uy

[<Flags>]
type FieldFlags =
    | ReadOnly = 0uy
    | Mutable = 0b0000_0001uy
    | Static = 0b0000_0010uy
    | ValidMask = 0b0000_0011uy

[<NoComparison; NoEquality>]
type Field =
    { FieldName: Name
      FieldFlags: FieldFlags
      FieldVisibility: VisibilityFlags
      FieldType: TypeIndex
      /// An array of annotations applied to the field.
      FieldAnnotations: vector<unit> }

[<Flags>]
type MethodFlags =
    | Final = 0uy
    | NotFinal = 0b0000_0001uy
    | Static = 0b0000_0010uy
    | ValidMask = 0b0000_0001uy

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type MethodBody =
    | Defined of MethodBodyIndex
    | Abstract
    ///| External

[<NoComparison; NoEquality>]
type Method =
    { MethodName: Name
      MethodFlags: MethodFlags
      TypeParameters: vector<unit>
      MethodVisibility: VisibilityFlags
      Signature: MethodSignature
      /// An array of annotations applied to the method.
      MethodAnnotations: vector<unit>
      Body: MethodBody }

/// Allows renaming of types in the metadata, similar to F#'s type abbreviations.
[<NoComparison; NoEquality>]
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

[<NoComparison; NoEquality>]
type TypeDefinitionKind =
    | Class of extends: TypeIndex * flags: ClassDefinitionFlags
    | Interface
    /// A type whose instances can only be allocated on the stack.
    | Struct

    member Kind : TypeDefinitionKindTag

type TypeDefinitionLayout =
    | Unspecified
    | Sequential
    ///| Explicit of

[<NoComparison; NoEquality>]
type TypeDefinition =
    { TypeName: Name
      TypeKind: TypeDefinitionKind
      TypeVisibility: VisibilityFlags
      TypeLayout: TypeDefinitionLayout
      ///ImplementedInterfaces: vector<unit>
      TypeParameters: vector<unit>
      /// An array of annotations applied to the type.
      TypeAnnotations: vector<unit>
      Fields: vector<Field>
      Methods: vector<unit> }

[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type NamespaceName =
    internal { Name: vector<Name> }

    override ToString : unit -> string

    interface IEquatable<NamespaceName>

val (|NamespaceName|) : name: NamespaceName -> vector<Name>

// TODO: Have function to turn a string "bleh.blah.bleh" into a vector<Name> for NamespaceName

[<NoComparison; NoEquality>]
type NamespaceImport =
    { NamespaceName: NamespaceName // TODO: Namespace name should be an identifier index.
      /// An array of the user-defined types imported from this namespace.
      TypeImports: LengthEncoded<vector<TypeDefinitionImport>>
      /// An array of the imported type aliases from this namespace.
      TypeAliases: LengthEncoded<vector<Name>> }

[<NoComparison; NoEquality>]
type Namespace =
    { NamespaceName: NamespaceName // TODO: Namespace name should be an identifier index.
      TypeDefinitions: LengthEncoded<vector<TypeDefinition>>
      TypeAliases: LengthEncoded<vector<TypeAlias>> }

[<NoComparison; StructuralEquality>]
type ModuleIdentifier =
    { ModuleName: Name
      Version: VersionNumbers }

    interface IEquatable<ModuleIdentifier>

[<NoComparison; NoEquality>]
type ModuleImport =
    { ImportedModule: ModuleIdentifier
      ImportedNamespaces: vector<NamespaceImport> }

[<Flags>]
type ModuleHeaderFlags =
    | LittleEndian = 0uy
    /// Indicates that numeric constants stored in the module data are in big-endian order.
    | BigEndian = 0b0000_0001uy
    /// Instructs the compiler or runtime to prevent usage of opcodes that depend on a garbage collector.
    | NoGarbageCollector = 0b0000_0010uy
    | ValidMask = 0b0000_0001uy

[<NoComparison; NoEquality>]
type ModuleHeader =
    { /// Specifies the name and version of this module.
      Module: ModuleIdentifier
      Flags: ModuleHeaderFlags
      /// The size, in bytes, of an unsafe pointer or object reference.
      PointerSize: uvarint }

    /// A LEB128 unsigned integer preceding the header indicating the number of fields in the header.
    member FieldCount: uvarint

[<NoComparison; NoEquality>]
type MethodBodyCode =
    { RegisterTypes: vector<TypeIndex>
      /// The instructions that make up the method body. Both the byte length and the actual number of instructions are included
      /// to simplify parsing.
      Instructions: LengthEncoded<vector<InstructionSet.Instruction>> }

    member RegisterCount : uvarint

type Debug = unit

[<NoComparison; NoEquality>]
type Module =
    { Magic: Magic
      FormatVersion: VersionNumbers
      Header: LengthEncoded<ModuleHeader>
      /// An array containing the names of the types, fields, and methods in this module.
      Identifiers: LengthEncoded<IdentifierSection>
      /// An array of structures describes the modules containing the types used by this module.
      Imports: LengthEncoded<vector<ModuleImport>>
      /// An array of byte arrays containing miscellaneous data such as the contents of string literals.
      Data: LengthEncoded<vector<vector<byte>>>
      /// An array of the namespaces defined in the module, which contain the module's types.
      Namespaces: LengthEncoded<vector<Namespace>>
      Code: LengthEncoded<vector<MethodBodyCode>>
      Debug: LengthEncoded<Debug> }

[<RequireQualifiedAccess>]
module UString =
    val ofStr : string -> UString

[<RequireQualifiedAccess>]
module Name =
    val tryOfStr : name: UString -> Name voption
