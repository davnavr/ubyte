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
val currentDataVectorCount : uvarint

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
    type [<Sealed; Class>] TypeDefinition = inherit Kind
    type [<Sealed; Class>] Method = inherit Kind
    type [<Sealed; Class>] Field = inherit Kind

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
type TypeDefinitionIndex = Index<IndexKinds.TypeDefinition>

/// <summary>
/// An index into the module's imported or defined methods. An index of <c>0</c> refers to the index of the first imported
/// method. The index of the first method defined in this module is equal to the number of imported methods.
/// </summary>
type MethodIndex = Index<IndexKinds.Method>

/// <summary>
/// An index into the module's imported or defined fields. An index of <c>0</c> refers to the index of the first imported field.
/// The index of the first field defined in this module is equal to the number of imported fields.
/// </summary>
type FieldIndex = Index<IndexKinds.Field>

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

        | incr = 0x2Cu

        | ``const.i32`` = 0x30u
        | ``const.i64`` = 0x31u
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
    type Instruction = // TODO: If efficiency is needed, can omit length integers from Ret and Call instructions
        | Nop
        /// <remarks>
        /// To simplify parsing, the number of registers containing the values to return is included as part of the instruction.
        /// </remarks>
        | Ret of vector<RegisterIndex>

        /// <summary>
        /// Calls the specified <paramref name="method"/> supplying the specified <paramref name="arguments"/> and storing the
        /// return values into the <paramref name="results"/> registers. For non-static methods, the <c>this</c> pointer is
        /// register <c>0</c>.
        /// </summary>
        /// <remarks>
        /// To simplify parsing, the number of argument registers is included as part of the instruction.
        /// </remarks>
        | Call of method: MethodIndex * arguments: vector<RegisterIndex> * results: vector<RegisterIndex> // TODO: How to ignore some return values? Maybe treat index 0 as ignore?

        /// <summary>
        /// Copies the value stored in the <paramref name="source"/> register to the <paramref name="destination"/> register.
        /// </summary>
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

        /// <summary>
        /// Stores a signed or unsigned 32-bit integer <paramref name="value"/> into the specified <paramref name="destination"/>
        /// register.
        /// </summary>
        /// <remarks>The integer is stored in the four bytes after the opcode, stored in the endianness as specified by the module header.</remarks>
        | Const_i32 of value: int32 * destination: RegisterIndex

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type IdentifierSection = // TODO: Rename to something else
    { Identifiers: vector<string> }

    member Item : index: IdentifierIndex -> string with get

[<RequireQualifiedAccess>]
module Tag =
    type MethodBody =
        | Defined = 0uy
        | Abstract = 1uy
        //| External = 2uy

    type TypeDefinitionKind =
        | BaseClass = 0uy
        | Class = 1uy
        | Interface = 2uy
        | Struct = 3uy

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
        | RefVector = 0xAuy
        | U8 = 0x10uy
        | U16 = 0x20uy
        | U32 = 0x40uy
        //| SNative = 0x49uy
        //| UNative = 0x55uy
        | U64 = 0x80uy
        /// Precedes a type index, represents a user-defined struct.
        | ValueType = 0xA1uy
        | RefAny = 0xAAuy
        | Bool = 0xB0uy
        /// Represents a UTF-16 code unit.
        | Char16 = 0xC2uy
        | Char32 = 0xC4uy
        /// Indicates that the following type instead represents a pointer to an instance of that type.
        | UnsafePointer = 0xCCuy
        /// Precedes a type index, represents an object reference to a user-defined type.
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

    interface IEquatable<PrimitiveType>

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type ReferenceType =
    | Defined of TypeDefinitionIndex
    | Any of AnyType // TODO: Remove this, and add BoxedPrimitive and BoxedValueType
    /// One-dimensional array whose first element is at index zero.
    | Vector of AnyType

    interface IEquatable<ReferenceType>

and [<NoComparison; StructuralEquality>] AnyType =
    | Primitive of PrimitiveType
    // TODO: Differentiate between object references and things that are similar to .NET byrefs
    //(e.g., will an object reference to some class in this system allow a value to be stored in it like a byref, or should a new type be added? Make a SafePointer type?)
    | ObjectReference of ReferenceType
    | UnsafePointer of AnyType
    /// User-defined struct that is passed by value. The index must point to a struct.
    | ValueType of TypeDefinitionIndex

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
      FieldType: TypeDefinitionIndex }

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
      Fields: vector<FieldIndex>
      Methods: vector<MethodIndex> }

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
    | Instance = 0b0000_0001uy
    | ValidMask = 0b0000_0001uy

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type MethodBody =
    | Defined of CodeIndex // TODO: Have flags indicating if method is final be here instead?
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
      AliasOf: TypeSignatureIndex }

[<Flags>]
type ClassDefinitionFlags =
    | Final = 0uy // TODO: Make final by default so "static" classes have to be marked abstract and final?
    /// The class can be inherited from.
    | NotFinal = 0b0000_0001uy
    /// Instances of this class cannot be created.
    | Abstract = 0b0000_0010uy
    | ValidMask = 0b0000_0011uy

[<NoComparison; NoEquality>]
type TypeDefinitionKind =
    | Class of extends: TypeDefinitionIndex voption * flags: ClassDefinitionFlags // TODO: Use TypeSignatureIndex to allow inheriting from generic types
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
      ImplementedInterfaces: vector<unit> //vector<TypeIndex>
      TypeParameters: vector<unit>
      /// An array of annotations applied to the type.
      TypeAnnotations: vector<unit>
      Fields: vector<FieldIndex>
      Methods: vector<MethodIndex> }

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
    | None = 0uy
    /// Indicates that the pointer stored in the register points to an object that is tracked by the garbage collector.
    | Pinned = 0b0000_0010uy
    // TODO: Have flag to allow using of registers before they are assigned to?
    | ValidMask = 0b0000_0000uy

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type RegisterType =
    { RegisterType: TypeSignatureIndex
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

type ModuleImport =
    { ImportedModule: ModuleIdentifier
      ImportedFields: vector<FieldImport>
      ImportedMethods: vector<MethodImport>
      ImportedNamespaces: vector<NamespaceImport> }

[<Flags>]
type ModuleHeaderFlags = // TODO: Have endian information be somewhere else, and allow endian-agnostic things.
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

[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type Endianness =
    | LittleEndian
    | BigEndian

[<NoComparison; NoEquality>]
type ModuleHeader =
    { /// Specifies the name and version of this module.
      Module: ModuleIdentifier
      Flags: ModuleHeaderFlags
      /// The size of an unsafe pointer or object reference.
      PointerSize: PointerSize }

    /// A LEB128 unsigned integer preceding the header indicating the number of fields in the header.
    member FieldCount: uvarint

    member internal Endianness : Endianness

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
      ////TypeAliases:
      //TypeDefinitions: // TODO: Should types be out here just like the fields and methods?
      Fields: LengthEncoded<vector<Field>>
      Methods: LengthEncoded<vector<Method>>
      /// An array of the namespaces defined in the module, which contain the module's types.
      Namespaces: LengthEncoded<vector<Namespace>>
      /// An optional index specifying the entry point method of the application.
      /// The entry point method must not have any type parameters. It is up to the compiler or runtime to determine if the
      /// signature of the entry point method is valid.
      EntryPoint: LengthEncoded<MethodIndex voption>
      Debug: LengthEncoded<Debug> }

    /// A LEB128 unsigned integer preceding the header indicating the number of data vectors that follow.
    member DataVectorCount: uvarint

    member Endianness : Endianness

[<RequireQualifiedAccess>]
module VersionNumbers =
    val semver : major: uvarint -> minor: uvarint -> patch: uvarint -> VersionNumbers
    val empty : VersionNumbers
    val ofValueOption : version: VersionNumbers voption -> VersionNumbers

[<RequireQualifiedAccess>]
module MethodSignature =
    val empty : MethodSignature

[<RequireQualifiedAccess>]
module Name =
    val tryOfStr : name: ustring -> Name voption

    /// <exception cref="T:System.ArgumentException">Thrown when the <paramref name="name"/> is empty.</exception>
    val ofStr : name: ustring -> Name
