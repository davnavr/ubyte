module UByte.Format.Model

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

[<RequireQualifiedAccess; IsReadOnly; Struct>]
type Magic = internal { Magic: ImmutableArray<byte> }

val (|Magic|) : magic: Magic -> ImmutableArray<byte>

val magic : Magic

/// Represents an array preceded by an LEB128 encoded unsigned integer indicating the number of items.
type vector<'T> = ImmutableArray<'T>

/// Represents a LEB128 encoded unsigned integer.
type uvarint = uint32

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
type ustring = string

/// Represents a length-encoded UTF-8 string that cannot be empty.
[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type Name =
    internal
    | Name of ustring

    override ToString: unit -> string

    interface IEquatable<Name>

val (|Name|) : name: Name -> ustring

/// Represents data that is preceded by a LEB128 unsigned integer indicating the byte length of the following data.
type LengthEncoded<'Data> = 'Data

[<RequireQualifiedAccess>]
module IndexKinds =
    type [<AbstractClass>] Kind = class end
    type [<Sealed; Class>] Identifier = inherit Kind
    type [<Sealed; Class>] TypeSignature = inherit Kind
    type [<Sealed; Class>] MethodSignature = inherit Kind
    type [<Sealed; Class>] Data = inherit Kind
    type [<Sealed; Class>] Code = inherit Kind
    type [<Sealed; Class>] Register = inherit Kind
    type [<Sealed; Class>] Type = inherit Kind
    type [<Sealed; Class>] Method = inherit Kind

[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type Index<'Kind when 'Kind :> IndexKinds.Kind> =
    | Index of uvarint

    interface IEquatable<Index<'Kind>>

/// <summary>An index into the module's identifiers. The index of the first identifier is <c>0</c>.</summary>
type IdentifierIndex = Index<IndexKinds.Identifier>

type TypeSignatureIndex = Index<IndexKinds.TypeSignature>

type MethodSignatureIndex = Index<IndexKinds.MethodSignature>

type DataIndex = Index<IndexKinds.Data>

/// <summary>An index into the module's method bodies. The index of the first method body is <c>0</c>.</summary>
type CodeIndex = Index<IndexKinds.Code>

/// <summary>
/// An index to a register. The index of the first register not representing a method parameter is equal to the number
/// of method parameters. The index of the first method parameter, if any are defined, is <c>0</c>.
/// </summary>
type RegisterIndex = Index<IndexKinds.Register>

/// <summary>
/// An index into the module's imported types or defined types. The index of the first imported type or type alias, if
/// any, is <c>0</c>. The index of the first type or type alias defined in this module is equal to the number of type aliases.
/// </summary>
type TypeIndex = Index<IndexKinds.Type>

/// <summary>
/// An index into the module's imported methods or defined methods. An index of <c>0</c> refers to the index of the
/// first imported method of the first imported type, if any are imported. The index of the first method defined in this module
/// is equal to the number of imported methods.
/// </summary>
type MethodIndex = Index<IndexKinds.Method>

module InstructionSet =
    /// Opcodes are represented as LEB128 encoded unsigned integers.
    type Opcode =
        | nop = 0u
        | ret = 1u

        | call = 0x10u




        // Register
        | ``reg.copy`` = 0x17u

        // Arithmetic
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

        // Branching
        | br = 0x40u
        | ``br.eq`` = 0x41u

    /// <remarks>
    /// Instructions that store integer constants into a register <c>const.</c> are followed by the integer constants in the
    /// endianness specified in the module header.
    /// </remarks>
    [<NoComparison; NoEquality>]
    type Instruction =
        | Nop
        | Ret of vector<RegisterIndex>

        /// <summary>
        /// Calls the specified <paramref name="method"/> supplying the specified <paramref name="arguments"/> and storing the
        /// return values into the <paramref name="results"/> registers. For non-static methods, the <c>this</c> pointer is
        /// register <c>0</c>.
        /// </summary>
        /// <remarks>
        /// The number of argument registers is not included in the instruction, and is instead determined by examining the
        /// signature of the method to be called.
        /// </remarks>
        | Call of method: MethodIndex * arguments: ImmutableArray<RegisterIndex> * results: vector<RegisterIndex>

        | Reg_copy of source: RegisterIndex * destination: RegisterIndex

        // Arithmetic
        /// <summary>
        /// Computes the sum of the values in registers <paramref name="x"/> and <paramref name="y"/> without an overflow check,
        /// and stores the sum in the <paramref name="result"/> register.
        /// </summary>
        | Add of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex

        /// <summary>
        /// Subtracts the value in register <paramref name="y"/> from register <paramref name="x"/> without an overflow check,
        /// and stores the difference in the <paramref name="result"/> register.
        /// </summary>
        | Sub of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex

        /// Stores a signed 32-bit integer into the specified register.
        | Const_s32 of value: int32 * destination: RegisterIndex

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type IdentifierSection =
    { Identifiers: vector<Name> }

    member Item : index: IdentifierIndex -> Name with get

[<RequireQualifiedAccess>]
module Tag =
    type TypeDefinitionKind =
        | Class = 0uy
        | Interface = 1uy
        | Struct = 2uy

    type TypeDefinitionLayout =
        | Unspecified = 0uy
        | Sequential = 1uy
        //| Explicit = 2uy

    type Type =
        | Unit = 0uy
        | S8 = 1uy
        | S16 = 2uy
        | S32 = 4uy
        | S64 = 8uy
        | Vector = 0xAuy
        | U8 = 0x10uy
        | U16 = 0x20uy
        | U32 = 0x40uy
        | U64 = 0x80uy
        | Bool = 0xB0uy
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
    | F32
    | F64
    | Unit

    interface IEquatable<PrimitiveType>

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type ReferenceType =
    | Defined of TypeIndex
    | Any of AnyType
    /// One-dimensional array whose first element is at index zero.
    | Vector of AnyType

    interface IEquatable<ReferenceType>

and [<NoComparison; StructuralEquality>] AnyType =
    | PrimitiveType of PrimitiveType
    | ObjectReference of ReferenceType
    | UnsafePointer of AnyType
    /// User-defined struct that is passed by value. The index must point to a struct.
    | ValueType of TypeIndex

    interface IEquatable<AnyType>

/// Describes the return types and parameter types of a method.
[<NoComparison; StructuralEquality>]
type MethodSignature =
    { /// The types of the values returned by the method.
      ReturnTypes: vector<TypeSignatureIndex>
      ParameterTypes: vector<TypeSignatureIndex> }

    interface IEquatable<MethodSignature>

[<NoComparison; NoEquality>]
type FieldImport =
    { FieldName: IdentifierIndex
      FieldType: TypeIndex }

[<NoComparison; NoEquality>]
type MethodImport =
    { MethodName: IdentifierIndex
      TypeParameters: uvarint
      Signature: MethodSignatureIndex }

[<NoComparison; NoEquality>]
type TypeDefinitionImport =
    { TypeName: IdentifierIndex
      TypeKind: Tag.TypeDefinitionKind
      TypeParameters: uvarint
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
    { FieldName: IdentifierIndex
      FieldVisibility: VisibilityFlags
      FieldFlags: FieldFlags
      FieldType: TypeSignatureIndex
      /// An array of annotations applied to the field.
      FieldAnnotations: vector<unit> }

[<Flags>]
type MethodFlags =
    | Final = 0uy
    | NotFinal = 0b0000_0001uy
    | Static = 0b0000_0010uy
    | ValidMask = 0b0000_0001uy

type MethodBodyTag =
    | Defined = 0uy
    | Abstract = 1uy
    //| External = 2uy

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type MethodBody =
    | Defined of CodeIndex
    | Abstract
    //| External of

[<NoComparison; NoEquality>]
type Method =
    { MethodName: IdentifierIndex
      MethodVisibility: VisibilityFlags
      MethodFlags: MethodFlags
      TypeParameters: vector<unit>
      Signature: MethodSignatureIndex
      /// An array of annotations applied to the method.
      MethodAnnotations: vector<unit>
      Body: MethodBody }

/// Allows renaming of types in the metadata, similar to F#'s type abbreviations.
[<NoComparison; NoEquality>]
type TypeAlias =
    { AliasName: IdentifierIndex
      AliasVisibility: VisibilityFlags
      AliasOf: AnyType }

[<Flags>]
type ClassDefinitionFlags =
    | Final = 0uy
    /// The class can be inherited from.
    | NotFinal = 0b0000_0001uy
    /// Instances of this class cannot be created.
    | Abstract = 0b0000_0010uy
    | ValidMask = 0b0000_0011uy

[<NoComparison; NoEquality>]
type TypeDefinitionKind =
    | Class of extends: TypeIndex * flags: ClassDefinitionFlags
    | Interface
    /// A type whose instances can be allocated on the stack.
    | Struct

[<RequireQualifiedAccess>]
type TypeDefinitionLayout =
    /// The runtime or compiler is free to decide how the fields of the class or struct are laid out.
    | Unspecified
    /// The fields of the class or struct are laid out sequentially.
    | Sequential
    //| Explicit of

[<NoComparison; NoEquality>]
type TypeDefinition =
    { TypeName: IdentifierIndex
      TypeVisibility: VisibilityFlags
      TypeKind: TypeDefinitionKind
      TypeLayout: TypeDefinitionLayout
      ImplementedInterfaces: vector<unit>
      TypeParameters: vector<unit>
      /// An array of annotations applied to the type.
      TypeAnnotations: vector<unit>
      Fields: vector<Field>
      Methods: vector<Method> }

    member KindTag : Tag.TypeDefinitionKind
    member LayoutTag : Tag.TypeDefinitionLayout

[<NoComparison; NoEquality>]
type NamespaceImport =
    { NamespaceName: vector<IdentifierIndex>
      /// An array of the user-defined types imported from this namespace.
      TypeImports: LengthEncoded<vector<TypeDefinitionImport>>
      /// An array of the imported type aliases from this namespace.
      TypeAliases: LengthEncoded<vector<IdentifierIndex>> }

[<NoComparison; NoEquality>]
type Namespace =
    { NamespaceName: vector<IdentifierIndex>
      TypeDefinitions: LengthEncoded<vector<TypeDefinition>>
      TypeAliases: LengthEncoded<vector<TypeAlias>> }

[<Flags>]
type RegisterFlags =
    /// Indicates that the pointer stored in the register points to an object that is tracked by the garbage collector.
    | Pinned = 0b0000_0010uy
    | ValidMask = 0b0000_0000uy

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type RegisterType =
    { RegisterType: TypeIndex
      RegisterFlags: RegisterFlags }

[<NoComparison; NoEquality>]
type Code =
    { /// The types and special flags for the registers of the method. A length precedes each register type and flag byte to
      /// allow easy duplication
      RegisterTypes: vector<struct(uvarint * RegisterType)>
      /// The instructions that make up the method body. Both the byte length and the actual number of instructions are included
      /// to simplify parsing.
      Instructions: LengthEncoded<vector<InstructionSet.Instruction>> }

    member RegisterCount : uvarint

type Debug = unit

[<NoComparison; StructuralEquality>]
type ModuleIdentifier =
    { ModuleName: Name
      Version: VersionNumbers }

    interface IEquatable<ModuleIdentifier>

[<NoComparison; StructuralEquality>]
type ModuleImport =
    { ImportedModule: ModuleIdentifier
      ImportedNamespaces: vector<NamespaceImport> }

    interface IEquatable<ModuleImport>

[<Flags>]
type ModuleHeaderFlags =
    | LittleEndian = 0uy
    /// Indicates that numeric constants stored in the module data are in big-endian order.
    | BigEndian = 0b0000_0001uy
    /// Instructs the compiler or runtime to prevent usage of opcodes that depend on a garbage collector.
    | NoGarbageCollector = 0b0000_0010uy
    | ValidMask = 0b0000_0001uy

type PointerSize =
    | Unspecified = 0uy
    | Is32Bit = 1uy
    | Is64Bit = 2uy

[<NoComparison; NoEquality>]
type ModuleHeader =
    { /// Specifies the name and version of this module.
      Module: ModuleIdentifier
      Flags: ModuleHeaderFlags
      /// The size of an unsafe pointer or object reference.
      PointerSize: PointerSize }

    /// A LEB128 unsigned integer preceding the header indicating the number of fields in the header.
    member FieldCount: uvarint

[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type Endianness =
    | LittleEndian
    | BigEndian

[<NoComparison; NoEquality>]
type Module =
    { Magic: Magic
      FormatVersion: VersionNumbers
      Header: LengthEncoded<ModuleHeader>
      /// An array containing the names of the types, fields, and methods in this module.
      Identifiers: LengthEncoded<IdentifierSection>
      /// An array of structures describes the modules containing the types used by this module.
      Imports: LengthEncoded<vector<ModuleImport>>
      TypeSignatures: LengthEncoded<vector<AnyType>>
      MethodSignatures: LengthEncoded<vector<MethodSignature>>
      /// An array of byte arrays containing miscellaneous data such as the contents of string literals.
      Data: LengthEncoded<vector<vector<byte>>>
      Code: LengthEncoded<vector<Code>>
      /// An array of the namespaces defined in the module, which contain the module's types.
      Namespaces: LengthEncoded<vector<Namespace>>
      /// An optional index specifying the entry point method of the application.
      /// The entry point method must not have any type parameters. It is up to the compiler or runtime to determine if the
      /// signature of the entry point method is valid.
      EntryPoint: LengthEncoded<MethodIndex voption>
      Debug: LengthEncoded<Debug> }

    member Endianness : Endianness

[<RequireQualifiedAccess>]
module Name =
    val tryOfStr : name: ustring -> Name voption
