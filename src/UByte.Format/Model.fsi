module UByte.Format.Model

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

[<RequireQualifiedAccess; IsReadOnly; Struct>]
type Magic = internal { Magic: ImmutableArray<byte> }

val (|Magic|) : magic: Magic -> ImmutableArray<byte>

val magic : Magic

/// Represents an array preceded by an variable-length unsigned integer indicating the number of items.
type vector<'T> = ImmutableArray<'T>

/// <summary>Represents a variable-length unsigned integer.</summary>
/// <remarks>
/// The variable-length integer is stored in little endian order, with the length of the integers specified by the number of
/// consecutive ones that are set in the lowest bits of the first byte that is written.
/// </remarks>
type uvarint = uint32

/// <summary>Represents a variable-length signed integer.</summary>
/// <remarks>
/// Variable-length signed integers are stored similarly to its unsigned counterpart. Signed integers are stored as a 7-bit two's
/// complement integer when in one byte, as a 14-bit two's complement integer when in two bytes, and so on.
/// </remarks>
type varint = int32

/// A length-encoded array of variable-length unsigned integers used to indicate a version.
[<IsReadOnly; Struct; CustomComparison; CustomEquality>]
type VersionNumbers =
    | VersionNumbers of vector<uvarint>

    override ToString : unit -> string

    override GetHashCode : unit -> int32

    interface IEquatable<VersionNumbers>
    interface IComparable
    interface IComparable<VersionNumbers>

val currentFormatVersion : VersionNumbers

[<Literal>]
val internal MaxDataVectorCount : uvarint = 11u

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

/// Represents data that is preceded by a variable-length unsigned integer indicating the byte length of the following data.
type LengthEncoded<'Data> = 'Data

[<RequireQualifiedAccess>]
module IndexKinds =
    type [<AbstractClass>] Kind = class end
    type [<Sealed; Class>] Identifier = inherit Kind
    type [<Sealed; Class>] Namespace = inherit Kind
    type [<Sealed; Class>] TypeSignature = inherit Kind
    type [<Sealed; Class>] MethodSignature = inherit Kind
    type [<Sealed; Class>] Module = inherit Kind
    type [<Sealed; Class>] TypeDefinition = inherit Kind
    type [<Sealed; Class>] Field = inherit Kind
    type [<Sealed; Class>] Method = inherit Kind
    type [<Sealed; Class>] Data = inherit Kind
    type [<Sealed; Class>] Code = inherit Kind
    type [<Sealed; Class>] Register = inherit Kind

[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type Index<'Kind when 'Kind :> IndexKinds.Kind> =
    | Index of uvarint

    interface IEquatable<Index<'Kind>>

/// <summary>An index into the module's identifiers. The index of the first identifier is <c>0</c>.</summary>
type IdentifierIndex = Index<IndexKinds.Identifier>

/// <summary>
/// An index into the namespaces of the types defined in this module, with the index of the first namespace being <c>0</c>.
/// </summary>
type NamespaceIndex = Index<IndexKinds.Namespace>

type TypeSignatureIndex = Index<IndexKinds.TypeSignature>

type MethodSignatureIndex = Index<IndexKinds.MethodSignature>

/// <summary>
/// <c>0</c> refers to the current module, while the index of the first imported module is <c>1</c>.
/// </summary>
type ModuleIndex = Index<IndexKinds.Module>

/// <summary>
/// An index into the module's imported types or defined types. The index of the first imported type or type alias, if
/// any, is <c>0</c>. The index of the first type or type alias defined in this module is equal to the number of type aliases.
/// </summary>
type TypeDefinitionIndex = Index<IndexKinds.TypeDefinition>

/// <summary>
/// An index into the module's imported or defined fields. An index of <c>0</c> refers to the index of the first imported field.
/// The index of the first field defined in this module is equal to the number of imported fields.
/// </summary>
type FieldIndex = Index<IndexKinds.Field>

/// <summary>
/// An index into the module's imported or defined methods. An index of <c>0</c> refers to the index of the first imported
/// method. The index of the first method defined in this module is equal to the number of imported methods.
/// </summary>
type MethodIndex = Index<IndexKinds.Method>

type DataIndex = Index<IndexKinds.Data>

/// <summary>An index into the module's method bodies. The index of the first method body is <c>0</c>.</summary>
type CodeIndex = Index<IndexKinds.Code>

/// <summary>
/// An index to a register. The index of the first register not representing a method parameter is equal to the number
/// of method parameters. The index of the first method parameter, if any are defined, is <c>0</c>.
/// </summary>
type RegisterIndex = Index<IndexKinds.Register>

module InstructionSet =
    /// <summary>
    /// Specifies the target of a branch instruction, points to the instruction that will be executed next if the branch is
    /// taken. <c>0</c> refers to the current branch instruction.
    /// </summary>
    type InstructionOffset = varint

    /// Opcodes are represented as variable-length encoded unsigned integers.
    type Opcode =
        | nop = 0u
        | ret = 1u
        | call = 5u
        | ``call.virt`` = 6u
        // ``call. = 7u
        | ``reg.copy`` = 0x17u
        //| ``global.ld`` = 0x19u
        //| ``global.st`` = 0x20u
        | add = 0x20u
        | sub = 0x21u
        | mul = 0x22u
        | div = 0x23u
        //|  = 27u
        //|  = 28u
        //|  = 29u
        //|  = 2Au
        //|  = 2Bu
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
        //|  = 36u
        //|  = 37u
        //|  = 38u
        //|  = 39u
        //|  = 3Au
        //|  = 3Bu
        //|  = 3Cu
        //|  = 3Du
        //|  = 3Eu
        //|  = 3Fu
        | ``and`` = 0x40u
        | ``or`` = 0x41u
        | ``not`` = 0x42u
        | ``xor`` = 0x43u
        | rem = 0x44u
        // TODO: Have operation that stores both division result AND remainder.
        //| = 0x45u
        //| = 0x46u
        //| = 0x47u
        // TODO: Have oepration for shifting integer bits left and right
        //| = 0x48u
        //| = 0x49u
        //| = 0x4Au
        //| = 0x4Bu
        // TODO: Have conversion operators (int to float, float to int, etc.) that differ from reg.copy, and variants with overflow checks
        | rotl = 0x4Cu
        | rotr = 0x4Du
        | br = 0x60u
        | ``br.eq`` = 0x61u
        | ``br.ne`` = 0x62u
        | ``br.lt`` = 0x63u
        | ``br.gt`` = 0x64u
        | ``br.le`` = 0x65u
        | ``br.ge`` = 0x66u
        | ``br.true`` = 0x67u
        | ``br.false`` = 0x68u
        | ``obj.new`` = 0x70u
        | ``obj.null`` = 0x71u
        | ``obj.ldfd`` = 0x72u
        | ``obj.stfd`` = 0x73u
        | ``obj.throw`` = 0x74u

        | ``obj.arr.new`` = 0x7Au
        | ``obj.arr.len`` = 0x7Bu
        | ``obj.arr.get`` = 0x7Cu
        //| ``obj.arr.addr`` = 0x7Du
        | ``obj.arr.set`` = 0x7Eu
        | ``obj.arr.const`` = 0x80u
        | ``call.ret`` = 0x90u
        | ``call.virt.ret`` = 0x91u
        | ``add.ovf`` = 0xA0u
        | ``sub.ovf`` = 0xA1u
        | ``mul.ovf`` = 0xA2u

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
        | Ret of results: vector<RegisterIndex>

        /// <summary>
        /// Calls the specified <paramref name="method"/> supplying the specified <paramref name="arguments"/> and storing the
        /// return values into the <paramref name="results"/> registers. For non-static methods, the <c>this</c> pointer is
        /// register <c>0</c>.
        /// </summary>
        /// <remarks>
        /// To simplify parsing, the number of argument registers is included as part of the instruction.
        /// </remarks>
        | Call of method: MethodIndex * arguments: vector<RegisterIndex> * results: vector<RegisterIndex> // TODO: How to ignore some return values? Maybe treat index 0 as ignore?
        | Call_virt of method: MethodIndex * arguments: vector<RegisterIndex> * results: vector<RegisterIndex>

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
        /// Subtracts the value in register <paramref name="y"/> from the value in register <paramref name="x"/> without an
        /// overflow check, and stores the difference in the <paramref name="result"/> register.
        /// </summary>
        | Sub of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        /// <summary>
        /// Computes the product of the values in registers <paramref name="x"/> and <paramref name="y"/> without an overflow
        /// check, and stores the product in the <paramref name="result"/> register.
        /// </summary>
        | Mul of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        /// <summary>
        /// Divides the value in register <paramref name="x"/> by the value in register <paramref name="y"/>, and stores the
        /// result in the <paramref name="result"/> register.
        /// </summary>
        | Div of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex

        /// <summary>
        /// Increments the value stored in the register by one, without an overflow check.
        /// </summary>
        | Incr of RegisterIndex
        | Incr_ovf of RegisterIndex

        /// <summary>
        /// Decrements the value stored in the register by one, without an overflow check.
        /// </summary>
        | Decr of RegisterIndex
        | Decr_ovf of RegisterIndex

        /// <summary>
        /// Stores a signed or unsigned 32-bit integer <paramref name="value"/> into the specified <paramref name="destination"/>
        /// register.
        /// </summary>
        /// <remarks>
        /// The integer is stored in the four bytes after the opcode, stored in the endianness as specified by the module header.
        /// </remarks>
        | Const_i32 of value: int32 * destination: RegisterIndex

        /// <summary>
        /// Stores a <see langword="true"/> value or the integer <c>1</c> into the specified register.
        /// </summary>
        | Const_true of RegisterIndex
        /// <summary>
        /// An alias for <c>const.zero</c>. stores a <see langword="false"/> value or the integer <c>0</c> into the specified
        /// register.
        /// </summary>
        | Const_false of RegisterIndex
        /// <summary>
        /// An alias for <c>const.false</c>, stores a value of <c>0</c> into the specified register.
        /// </summary>
        | Const_zero of RegisterIndex

        /// <summary>
        /// Computes the bitwise <c>AND</c> of the values stored in registers <paramref name="x"/> and <paramref name="y"/> and
        /// stores the result in the <paramref name="result"/> register.
        /// </summary>
        | And of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        /// <summary>
        /// Computes the bitwise <c>OR</c> of the values stored in registers <paramref name="x"/> and <paramref name="y"/> and
        /// stores the result in the <paramref name="result"/> register.
        /// </summary>
        | Or of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        /// <summary>
        /// Computes the bitwise <c>NOT</c> of the values stored in registers <paramref name="x"/> and <paramref name="y"/> and
        /// stores the result in the <paramref name="result"/> register.
        /// </summary>
        | Not of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        /// <summary>
        /// Computes the bitwise <c>XOR</c> of the values stored in registers <paramref name="x"/> and <paramref name="y"/> and
        /// stores the result in the <paramref name="result"/> register.
        /// </summary>
        | Xor of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        /// <summary>
        /// Divides the value in register <paramref name="x"/> by the value in register <paramref name="y"/>, and stores the
        /// remainder in the <paramref name="result"/> register.
        /// </summary>
        | Rem of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex

        | Rotl of amount: RegisterIndex * RegisterIndex
        | Rotr of amount: RegisterIndex * RegisterIndex

        /// <summary>
        /// Performs an unconditional branch to the instruction specified by the <paramref name="target"/> offset.
        /// </summary>
        | Br of target: InstructionOffset
        /// <summary>
        /// If the value in register <paramref name="x"/> is equal to the value in register <paramref name="y"/>, branches to the
        /// instruction specified by the <paramref name="target"/> offset.
        /// </summary>
        | Br_eq of x: RegisterIndex * y: RegisterIndex * target: InstructionOffset
        /// <summary>
        /// If the value in register <paramref name="x"/> is not equal to the value in register <paramref name="y"/>, branches to
        /// the instruction specified by the <paramref name="target"/> offset.
        /// </summary>
        | Br_ne of x: RegisterIndex * y: RegisterIndex * target: InstructionOffset
        /// <summary>
        /// If the value in register <paramref name="x"/> is less than the value in register <paramref name="y"/>, branches to
        /// the instruction specified by the <paramref name="target"/> offset.
        /// </summary>
        | Br_lt of x: RegisterIndex * y: RegisterIndex * target: InstructionOffset
        /// <summary>
        /// If the value in register <paramref name="x"/> is greater than the value in register <paramref name="y"/>, branches to
        /// the instruction specified by the <paramref name="target"/> offset.
        /// </summary>
        | Br_gt of x: RegisterIndex * y: RegisterIndex * target: InstructionOffset
        /// <summary>
        /// If the value in register <paramref name="x"/> is less than or equal to the value in register <paramref name="y"/>,
        /// branches to the instruction specified by the <paramref name="target"/> offset.
        /// </summary>
        | Br_le of x: RegisterIndex * y: RegisterIndex * target: InstructionOffset
        /// <summary>
        /// If the value in register <paramref name="x"/> is greater than or equal to the value in register <paramref name="y"/>,
        /// branches to the instruction specified by the <paramref name="target"/> offset.
        /// </summary>
        | Br_ge of x: RegisterIndex * y: RegisterIndex * target: InstructionOffset
        /// <summary>
        /// If the value in register <paramref name="x"/> is not equal to <c>0</c>, branches to the instruction specified by the
        /// <paramref name="target"/> offset.
        /// </summary>
        | Br_true of x: RegisterIndex * target: InstructionOffset
        /// <summary>
        /// If the value in register <paramref name="x"/> is equal to <c>0</c>, branches to the instruction specified by the
        /// <paramref name="target"/> offset.
        /// </summary>
        | Br_false of x: RegisterIndex * target: InstructionOffset
        //| Br_zero

        /// <summary>
        /// Allocates a new object and calls the specified <paramref name="constructor"/> with the specified
        /// <paramref name="arguments"/> and stores an object reference to the object into the <paramref name="result"/>
        /// register.
        /// </summary>
        | Obj_new of constructor: MethodIndex * arguments: vector<RegisterIndex> * result: RegisterIndex
        /// <summary>
        /// Stores a <see langword="null"/> object reference into the specified <paramref name="destination"/> register.
        /// </summary>
        | Obj_null of destination: RegisterIndex
        /// <summary>
        /// Copies the value of an object's field into the <paramref name="destination"/> register.
        /// </summary>
        | Obj_ldfd of field: FieldIndex * object: RegisterIndex * destination: RegisterIndex
        /// <summary>
        /// Copies the value from the <paramref name="source"/> register into the field of the specified object.
        /// </summary>
        | Obj_stfd of field: FieldIndex * object: RegisterIndex * source: RegisterIndex

        /// <summary>
        /// Allocates a new vector containing elements of the specified type with the specified <paramref name="length"/> and
        /// stores an object reference to the vector into the <paramref name="result"/> register.
        /// </summary>
        | Obj_arr_new of etype: TypeSignatureIndex * length: RegisterIndex * result: RegisterIndex
        /// <summary>
        /// Stores the length of the specified <paramref name="array"/> into the <paramref name="result"/> register.
        /// </summary>
        | Obj_arr_len of array: RegisterIndex * result: RegisterIndex
        | Obj_arr_get of array: RegisterIndex * index: RegisterIndex * result: RegisterIndex

        | Obj_arr_set of array: RegisterIndex * index: RegisterIndex * source: RegisterIndex
        /// <summary>
        /// Allocates a new vector containing elements of the specified type from the specified module <paramref name="data"/>
        /// and stores an object reference to the vector into the <paramref name="result"/> register.
        /// </summary>
        | Obj_arr_const of etype: TypeSignatureIndex * data: DataIndex * result: RegisterIndex

        | Call_ret of method: MethodIndex * arguments: vector<RegisterIndex> * results: vector<RegisterIndex>
        | Call_virt_ret of method: MethodIndex * arguments: vector<RegisterIndex> * results: vector<RegisterIndex>
        | Add_ovf of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        /// <summary>
        /// Subtracts the value in register <paramref name="y"/> from the value in register <paramref name="x"/> with an
        /// overflow check, and stores the difference in the <paramref name="result"/> register.
        /// </summary>
        | Sub_ovf of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex
        | Mul_ovf of x: RegisterIndex * y: RegisterIndex * result: RegisterIndex

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type IdentifierSection = // TODO: Rename to something else
    { Identifiers: vector<string> }

    member Item : index: IdentifierIndex -> string with get

[<RequireQualifiedAccess>]
module Tag =
    type MethodBody =
        | Defined = 0uy
        | Abstract = 1uy
        | External = 2uy

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
        /// Represents an object reference to an array containing elements of the following type.
        | RefVector = 0xAuy
        | U8 = 0x10uy
        | U16 = 0x20uy
        | U32 = 0x40uy
        | SNative = 0x49uy
        | UNative = 0x55uy
        | U64 = 0x80uy
        /// Precedes a type index, represents a user-defined struct.
        | DefinedStruct = 0xA1uy
        | RefAny = 0xAAuy
        | Bool = 0xB0uy
        /// Represents an object reference to a boxed instance of the following value type.
        | RefBoxed = 0xBBuy
        /// Represents a UTF-16 code unit.
        | Char16 = 0xC2uy
        | Char32 = 0xC4uy
        /// Represents a pointer to an instance of the following value type.
        | UnsafePointer = 0xCCuy
        | SafePointer = 0xCEuy
        | ObjectPointer = 0xCFuy
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
    | UNative
    | SNative
    | Char16
    | Char32
    | F32
    | F64
    | Unit

    interface IEquatable<PrimitiveType>

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type ValueType =
    | Primitive of PrimitiveType
    /// User-defined struct that is passed by value. The types index must point to a struct.
    | Defined of TypeDefinitionIndex
    | UnsafePointer of ValueType

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type ReferenceType =
    | Defined of TypeDefinitionIndex
    | BoxedValueType of ValueType
    /// <summary>
    /// Represents an untyped object reference, similar to <see cref="T:System.Object"/> in the Common Language Runtime.
    /// </summary>
    | Any
    /// One-dimensional array whose first element is at index zero.
    | Vector of ReferenceOrValueType

    interface IEquatable<ReferenceType>

and [<RequireQualifiedAccess; NoComparison; StructuralEquality>] ReferenceOrValueType =
    | Reference of ReferenceType
    | Value of ValueType

[<NoComparison; StructuralEquality>]
type AnyType =
    | ValueType of ValueType
    | ReferenceType of ReferenceType
    /// Represents a pointer to a field, array element, or register that is tracked by the garbage collector.
    | SafePointer of ReferenceOrValueType
    ///// Represents a pointer to a structure containing the actual value as well as information allowing dynamic method dispatch.
    //| ObjectPointer of TypeDefinitionIndex

    interface IEquatable<AnyType>

/// Describes the return types and parameter types of a method.
[<NoComparison; StructuralEquality>]
type MethodSignature =
    { /// <summary>The types of the values returned by the method.</summary>
      /// <remarks>
      /// Methods that return multiple values can easily be translated to a .NET method with <see langword="out"/> parameters.
      /// </remarks>
      ReturnTypes: vector<TypeSignatureIndex>
      ParameterTypes: vector<TypeSignatureIndex> }

    interface IEquatable<MethodSignature>

[<NoComparison; NoEquality>]
type FieldImport =
    { // NOTE: Field and method imports allow refering to types defined in the current model,
      // which makes adding generics in the future easier.
      FieldOwner: TypeDefinitionIndex
      FieldName: IdentifierIndex
      FieldType: TypeSignatureIndex }

[<NoComparison; NoEquality>]
type MethodImport =
    { MethodOwner: TypeDefinitionIndex
      MethodName: IdentifierIndex // TODO: How to handle importing constructors?
      TypeParameters: uvarint // TODO: Provide list of type parameters instead.
      Signature: MethodSignatureIndex }

[<NoComparison; NoEquality>]
type TypeDefinitionImport =
    { Module: ModuleIndex
      TypeName: IdentifierIndex
      TypeNamespace: NamespaceIndex
      TypeParameters: uvarint }

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
    { FieldOwner: TypeDefinitionIndex
      FieldName: IdentifierIndex
      FieldVisibility: VisibilityFlags
      FieldFlags: FieldFlags
      FieldType: TypeSignatureIndex
      /// An array of annotations applied to the field.
      FieldAnnotations: vector<unit> }

[<Flags>]
type MethodFlags =
    | Final = 0uy
    | Instance = 0b0000_0001uy
    | Constructor = 0b0000_0010uy
    | ConstructorMask = 0b0000_0011uy
    | ValidMask = 0b0000_0011uy

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type MethodBody =
    /// Defined in the current module.
    | Defined of CodeIndex // TODO: Have flags indicating if method is final be here instead?
    /// Not defined in the current type, but in a derived type.
    | Abstract
    /// Defined elsewhere, used by the foreign function interface or to call methods defined in the runtime.
    | External of library: IdentifierIndex * entryPointFunction: IdentifierIndex

/// <summary>
/// Represents a method or constructor.
/// </summary>
/// <remarks>
/// Valid constructors must have the <c>Instance</c> and <c>Constructor</c> flags set, must have no type parameters, and must
/// not have any return values.
/// </remarks>
[<NoComparison; NoEquality>]
type Method =
    { MethodOwner: TypeDefinitionIndex
      MethodName: IdentifierIndex
      MethodVisibility: VisibilityFlags
      MethodFlags: MethodFlags
      TypeParameters: vector<unit>
      Signature: MethodSignatureIndex
      /// An array of annotations applied to the method.
      MethodAnnotations: vector<unit>
      Body: MethodBody }

[<Flags>]
type TypeDefinitionFlags =
    | Final = 0uy
    /// The type can be inherited from.
    | NotFinal = 0b0000_0001uy
    /// Instances of this type cannot be created.
    | Abstract = 0b0000_0010uy
    /// Instances of this type can never be allocated on the stack.
    | ReferenceOnly = 0b0000_0100uy
    /// Instances of this type can only be allocated on the stack.
    | StackOnly = 0b0000_1000uy
    | StorageKindMask = 0b0000_1100uy
    | ValidMask = 0b0000_1111uy

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
      TypeNamespace: NamespaceIndex
      TypeVisibility: VisibilityFlags
      TypeFlags: TypeDefinitionFlags
      TypeLayout: TypeDefinitionLayout
      TypeParameters: vector<unit>
      InheritedTypes: vector<TypeDefinitionIndex>
      /// An array of annotations applied to the type.
      TypeAnnotations: vector<unit>
      Fields: vector<FieldIndex>
      Methods: vector<MethodIndex>
      ///// <summary>
      ///// The initializer that must be run before the first time a static field, static method, or constructor defined
      ///// in this class is used.
      ///// </summary>
      ///// <remarks>
      ///// Type definition initializers are analagous to <c>static</c> constructors in C#.
      ///// </remarks>
      //Initializers: vector<InitializerIndex voption>
      }

[<Flags>]
type RegisterFlags =
    | None = 0uy
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

[<Flags>]
type ModuleHeaderFlags = // TODO: Have endian information be somewhere else, and allow endian-agnostic things.
    | LittleEndian = 0uy
    /// Indicates that numeric constants stored in the module data are in big-endian order.
    | BigEndian = 0b0000_0001uy
    /// Instructs the compiler or runtime to prevent usage of opcodes that depend on a garbage collector.
    | NoGarbageCollector = 0b0000_0010uy
    ///// Indicates that the module may treat object references as if they were references to objects of another class.
    //| ReinterpretsObjectReferences = 0b0000_0100uy
    | ValidMask = 0b0000_0001uy

type PointerSize =
    | Unspecified = 0uy
    | Is32Bit = 1uy
    | Is64Bit = 2uy

[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type Endianness =
    | LittleEndian
    | BigEndian

/// Describes the identity of a module and its properties.
[<NoComparison; NoEquality>]
type ModuleHeader =
    { /// Specifies the name and version of this module.
      Module: ModuleIdentifier
      Flags: ModuleHeaderFlags
      /// The size of an unsafe pointer or object reference.
      PointerSize: PointerSize }

    /// A variable-length unsigned integer preceding the header indicating the number of fields in the header.
    member FieldCount: uvarint

    member internal Endianness : Endianness

[<NoComparison; NoEquality>]
type ModuleImports =
    { ImportedModules: vector<ModuleIdentifier>
      ImportedTypes: LengthEncoded<vector<TypeDefinitionImport>>
      ImportedFields: LengthEncoded<vector<FieldImport>>
      ImportedMethods: LengthEncoded<vector<MethodImport>> }

/// <summary>Contains the types, fields, and methods defined in the module.</summary>
/// <remarks>
/// Each type contains a list indices refering to the fields and methods that it defines, and each field or method contains the
/// index of the type that defines it. These indices must match in order for the module to be valid.
/// </remarks>
[<NoComparison; NoEquality>]
type ModuleDefinitions =
    { DefinedTypes: LengthEncoded<vector<TypeDefinition>>
      DefinedFields: LengthEncoded<vector<Field>>
      /// The methods defined in the module, all methods are stored in an array for efficient retrieval of method information,
      /// which is needed when invoking the entrypoint of a module.
      DefinedMethods: LengthEncoded<vector<Method>>
      //DefinedInitializers: LengthEncoded<vector<TypeInitializer>>
      }

[<NoComparison; NoEquality>]
type Module =
    { Magic: Magic
      FormatVersion: VersionNumbers
      /// The header, which identifies the module and its properties.
      Header: LengthEncoded<ModuleHeader>
      /// An array containing the names of the types, fields, and methods.
      Identifiers: LengthEncoded<IdentifierSection>
      /// An array of the namespaces containing the imported and defined types.
      Namespaces: LengthEncoded<vector<vector<IdentifierIndex>>>
      TypeSignatures: LengthEncoded<vector<AnyType>>
      MethodSignatures: LengthEncoded<vector<MethodSignature>>
      Imports: LengthEncoded<ModuleImports>
      Definitions: LengthEncoded<ModuleDefinitions>
      /// An array of byte arrays containing miscellaneous data such as the contents of string literals.
      Data: LengthEncoded<vector<vector<byte>>>
      Code: LengthEncoded<vector<Code>>
      /// An optional index specifying the entry point method of the application. The entry point method must not have any type
      /// parameters. It is up to the compiler or runtime to determine if the signature of the entry point method is valid.
      EntryPoint: LengthEncoded<MethodIndex voption>
      Debug: LengthEncoded<Debug> }

    /// A variable-length unsigned integer preceding the header indicating the number of data vectors that follow.
    member DataVectorCount: uvarint

    member Endianness : Endianness

[<RequireQualifiedAccess>]
module VersionNumbers =
    val semver : major: uvarint -> minor: uvarint -> patch: uvarint -> VersionNumbers
    val empty : VersionNumbers
    val ofValueOption : version: VersionNumbers voption -> VersionNumbers
    val ofList : version: uvarint list -> VersionNumbers

[<RequireQualifiedAccess>]
module MethodSignature =
    val empty : MethodSignature

[<RequireQualifiedAccess>]
module Name =
    val tryOfStr : name: ustring -> Name voption

    /// <exception cref="T:System.ArgumentException">Thrown when the <paramref name="name"/> is empty.</exception>
    val ofStr : name: ustring -> Name
