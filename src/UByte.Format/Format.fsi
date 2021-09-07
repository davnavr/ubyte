module UByte.Format.Format

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text

[<RequireQualifiedAccess; IsReadOnly; Struct>]
type Magic = internal { Magic: ImmutableArray<byte> }

val (|Magic|) : magic: Magic -> ImmutableArray<byte>

val magic : Magic

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
type Section<'Data> = 'Data

[<RequireQualifiedAccess>]
module IndexKinds =
    type [<AbstractClass>] Kind = class end
    type [<Sealed; Class>] Identifier = inherit Kind
    type [<Sealed; Class>] Type = inherit Kind
    type [<Sealed; Class>] MethodBody = inherit Kind

[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type Index<'Kind when 'Kind :> IndexKinds.Kind> =
    | Index of uvarint

    interface IEquatable<Index<'Kind>>

/// Represents an index into the module's identifiers. The index of the first identifier is 0.
type IdentifierIndex = Index<IndexKinds.Identifier>

type MethodBodyIndex = Index<IndexKinds.MethodBody>

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
      ReturnType: vector<AnyType>
      ParameterTypes: vector<AnyType> }

    interface IEquatable<MethodSignature>

[<NoComparison; NoEquality>]
type FieldImport =
    { FieldName: Name
      FieldType: AnyType }

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
      FieldType: AnyType
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
      AliasOf: AnyType } // TODO: Consider using index to point to an AnyType for defined TypeAliases and have a Section<vector<AnyType>>. Or maybe just use TypeIndices to point to TypeAliases (do the latter!)

[<Flags>]
type ClassDefinitionFlags =
    | Final = 0uy
    | NotFinal = 0b0000_0001uy
    | Abstract = 0b0000_0010uy
    | ValidMask = 0b0000_0011uy

[<NoComparison; NoEquality>]
type TypeDefinitionKind =
    | Class of extends: Index<unit> * flags: ClassDefinitionFlags
    | Interface
    /// A type whose instances can only be allocated on the stack.
    | Struct

    member Kind : TypeDefinitionKindTag

type TypeDefinitionLayout =
    | Unspecified
    | Sequential
    ///| Explicit

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
    { NamespaceName: NamespaceName
      /// An array of the user-defined types imported from this namespace.
      TypeImports: Section<vector<TypeDefinitionImport>>
      /// An array of the imported type aliases from this namespace.
      TypeAliases: Section<vector<Name>> }

[<NoComparison; NoEquality>]
type Namespace =
    { NamespaceName: NamespaceName
      TypeDefinitions: Section<vector<TypeDefinition>>
      TypeAliases: Section<vector<TypeAlias>> }

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

[<NoComparison; NoEquality>]
type MethodBodyCode =
    { Locals: vector<AnyType> }

/// Contains the names for methods parameters and local variables.
type DebugNames = unit

[<NoComparison; NoEquality>]
type Module =
    { Magic: Magic
      FormatVersion: VersionNumbers
      Header: ModuleHeader
      /// An array containing the names of the types, fields, and methods in this module.
      Identifiers: Section<IdentifierSection>
      /// An array of structures describes the modules containing the types used by this module.
      Imports: Section<vector<ModuleImport>>
      /// An array of byte arrays containing miscellaneous data such as the contents of string literals.
      Data: Section<vector<vector<byte>>>
      /// An array of the namespaces defined in the module, which contain the module's types.
      Namespaces: Section<vector<Namespace>>
      Code: Section<vector<MethodBodyCode>>
      DebugNames: Section<DebugNames> }

[<RequireQualifiedAccess>]
module UString =
    val ofStr : string -> UString

[<RequireQualifiedAccess>]
module Name =
    val tryOfStr : name: UString -> Name voption
