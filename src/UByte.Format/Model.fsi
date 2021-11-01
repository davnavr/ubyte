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
    type [<Sealed; Class>] Block = inherit Kind
    type [<Sealed; Class>] TemporaryRegister = inherit Kind
    type [<Sealed; Class>] LocalRegister = inherit Kind
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

type CodeBlockIndex = Index<IndexKinds.Block>

type TemporaryIndex = Index<IndexKinds.TemporaryRegister>

type LocalIndex = Index<IndexKinds.LocalRegister>

/// <summary>
/// An index to a register. The mapping of indices to registers is as follows: temporary registers, argument registers,
/// local registers.
/// </summary>
type RegisterIndex = Index<IndexKinds.Register>

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
    | Unit // TODO: Remove unit type, it should be in a standard library instead.

    interface IEquatable<PrimitiveType>

module InstructionSet =
    /// <summary>
    /// Specifies the target of a branch instruction, points to the block containing the instructions that will be executed next
    /// if the target branch is taken. <c>0</c> refers to the current block.
    /// </summary>
    type BlockOffset = varint

    /// Opcodes are represented as variable-length unsigned integers.
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
        //| = 0x31 TODO: Have operation that stores both division result AND remainder.
        // TODO: Have oepration for shifting integer bits left and right
        //| = 0x32u
        //| = 0x33u
        | rotl = 0x34u
        | rotr = 0x35u
        | ``const.s`` = 0x40u
        | ``const.u`` = 0x41u
        | ``const.f32`` = 0x42u
        | ``const.f64`` = 0x43u
        | ``const.true`` = 0x44u
        | ``const.zero`` = 0x45u
        | ``const.false`` = 0x45u
        // TODO: Have conversion operators (int to float, float to int, etc.), with ArithmeticFlags for overflow checks.
        //|  = 46u
        //|  = 47u
        //|  = 48u
        //|  = 49u
        //|  = 4Au
        //|  = 4Bu
        //|  = 4Cu
        //|  = 4Du
        //|  = 4Eu
        //|  = 4Fu
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
        | ``obj.arr.addr`` = 0x7Du
        | ``obj.arr.set`` = 0x7Eu
        | ``obj.arr.const`` = 0x7Fu

        | alloca = 0xAAu
        | ``alloca.obj`` = 0xABu

    [<Flags>]
    type ArithmeticFlags =
        | None = 0uy
        | ThrowOnOverflow = 1uy
        /// <summary>
        /// Applicable only to the <c>div</c> and <c>rem</c> instructions, indicates that an exception should be thrown if the
        /// denominator is zero.
        /// </summary>
        | ThrowOnDivideByZero = 1uy
        | ValidMask = 1uy

    [<Flags>]
    type CallFlags =
        | None = 0uy
        | NoTailCallOptimization = 1uy
        /// Indicates that tail call optimization must occur, and that the call instruction must immediately precede a <c>ret</c>
        /// instruction that returns all of the values returned by the called method.
        | RequiresTailCallOptimization = 2uy
        /// <summary>
        /// Applicable only to the <c>call.virt</c> instruction, indicates that exception should be thrown if the <c>this</c>
        /// object reference is <see langword="null"/>.
        /// </summary>
        | ThrowOnNullThis = 0b1000_0000uy

    [<Flags>]
    type AllocationFlags =
        | None = 0uy
        | ThrowOnFailure = 1uy

    [<Flags>]
    type MemoryAccessFlags =
        | None = 0uy
        | ThrowOnInvalidAccess = 1uy
        | AllowUnaligned = 2uy

    /// <remarks>
    /// <para>
    /// Instructions that store integer constants into a register <c>const.</c> are followed by the integer constants in the
    /// endianness specified in the module header.
    /// </para>
    /// <para>
    /// For instructions that take a vector of registers, such as <c>ret</c> or <c>call</c>, the length of the vector is
    /// included as usual to simplify parsing.
    /// </para>
    /// <para>
    /// For instructions that call another method, such as <c>call</c> or <c>call.virt</c>, the number of registers used as
    /// arguments must exactly match the number of arguments specified by the signature of the method. Additionally, the number
    /// of temporary registers introduced is equal to the number of return values.
    /// </para>
    /// </remarks>
    /// <seealso cref="T:UByte.Format.Model.InstructionSet.CallFlags" />
    [<NoComparison; NoEquality>]
    type Instruction =
        // NOTE: If efficiency is needed, could theoretically omit length integers from Ret and Call instructions
        // TODO: Add FieldIndex/RegisterIndex that points to exception to throw in the event that an instruction throws an exception (e.g. add throw.ovf @my_exception_value ...)
        /// <summary>
        /// <para>
        /// <c>nop</c>
        /// </para>
        /// <para>
        /// Does absolutely nothing.
        /// </para>
        /// </summary>
        | Nop
        /// <summary>
        /// <para>
        /// <c>ret (&lt;values&gt;)</c>
        /// </para>
        /// <para>
        /// Returns the values in the specified registers and transfers control back to the calling method, must be the last
        /// instruction in a block.
        /// </para>
        /// </summary>
        | Ret of results: vector<RegisterIndex> // TODO: How to ignore some return values? Maybe treat index 0 as ignore?
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = phi &lt;register0&gt; when &lt;block0&gt; or &lt;register1&gt; when &lt;block1&gt; or ...</c>
        /// </para>
        /// <para>
        /// Returns the value contained in the corresponding register depending on the previous block that was executed.
        /// </para>
        /// </summary>
        | Phi of vector<struct(RegisterIndex * BlockOffset)>
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = select &lt;condition&gt; then &lt;vtrue&gt; else &lt;vfalse&gt; </c>
        /// </para>
        /// <para>
        /// Returns the value in the <paramref name="vtrue"/> register if the value in the <paramref name="condition"/> register
        /// is <see langword="true"/>; otherwise, returns the value in the <paramref name="vfalse"/> register.
        /// </para>
        /// </summary>
        | Select of condition: RegisterIndex * vtrue: RegisterIndex * vfalse: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>(&lt;result0&gt;, &lt;result1&gt;, ...) = call [tail.prohibited | tail.required] &lt;method&gt; (&lt;argument0&gt;, &lt;argument1&gt;, ...)</c>
        /// </para>
        /// <para>
        /// Calls the specified <paramref name="method"/> with the specified <paramref name="arguments"/>.
        /// </para>
        /// </summary>
        | Call of CallFlags * method: MethodIndex * arguments: vector<RegisterIndex> //* results: vector<uvarint>
        /// <summary>
        /// <para>
        /// <c>(&lt;result0&gt;, &lt;result1&gt;, ...) = call.virt [tail.prohibited | tail.required] [throw.nullthis] &lt;method&gt; &lt;this&gt; (&lt;argument1&gt;, &lt;argument2&gt;, ...)</c>
        /// </para>
        /// <para>
        /// Calls an instance <paramref name="method"/> based on the type of the <paramref name="this"/> object
        /// reference.
        /// </para>
        /// </summary>
        | Call_virt of CallFlags * method: MethodIndex * this: RegisterIndex * arguments: vector<RegisterIndex>
        //| Call_indr of CallFlags * method: RegisterIndex * arguments: vector<RegisterIndex>
        //| Global_ld
        //| Global_st
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = add [throw.ovf] &lt;numeric type&gt; &lt;x&gt; &lt;y&gt;</c>
        /// </para>
        /// <para>
        /// Computes the sum of the values in registers <paramref name="x"/> and <paramref name="y"/> converted to the specified
        /// numeric type. and returns the sum.
        /// </para>
        /// </summary>
        | Add of ArithmeticFlags * PrimitiveType * x: RegisterIndex * y: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = sub [throw.ovf] &lt;numeric type&gt; &lt;x&gt; &lt;y&gt;</c>
        /// </para>
        /// <para>
        /// Converts the values in the <paramref name="x"/> and <paramref name="y"/> registers to the specified numeric type,
        /// subtracts the value in register <paramref name="y"/> from the value in register <paramref name="x"/>, returns the
        /// difference.
        /// </para>
        /// </summary>
        | Sub of ArithmeticFlags * PrimitiveType * x: RegisterIndex * y: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = mul [throw.ovf] &lt;numeric type&gt; &lt;x&gt; &lt;y&gt;</c>
        /// </para>
        /// <para>
        /// Computes the product of the values in registers <paramref name="x"/> and <paramref name="y"/> converted to the
        /// specified numeric type, and returns the product.
        /// </para>
        /// </summary>
        | Mul of ArithmeticFlags * PrimitiveType * x: RegisterIndex * y: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = div [throw.div0] &lt;numeric type&gt; &lt;x&gt; &lt;y&gt;</c>
        /// </para>
        /// <para>
        /// Converts the values in the <paramref name="x"/> and <paramref name="y"/> registers to the specified numeric type,
        /// divides the value in register <paramref name="x"/> by the value in register <paramref name="y"/>, and returns the result.
        /// </para>
        /// </summary>
        | Div of ArithmeticFlags * PrimitiveType * x: RegisterIndex * y: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = incr [throw.ovf] &lt;numeric type&gt; &lt;value&gt;</c>
        /// </para>
        /// <para>
        /// Converts the value stored in the register to the specified numeric type, and returns the value plus one.
        /// </para>
        /// </summary>
        | Incr of ArithmeticFlags * PrimitiveType * RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = decr [throw.ovf] &lt;numeric type&gt; &lt;value&gt;</c>
        /// </para>
        /// <para>
        /// Converts the value stored in the register to the specified numeric type, and returns the value minus one.
        /// </para>
        /// </summary>
        | Decr of ArithmeticFlags * PrimitiveType * RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = and &lt;integer type&gt; &lt;x&gt; &lt;y&gt;</c>
        /// </para>
        /// <para>
        /// Converts the values in the <paramref name="x"/> and <paramref name="y"/> registers to the specified integer type,
        /// computes the bitwise <c>AND</c>, and returns the result.
        /// </para>
        /// </summary>
        | And of PrimitiveType * x: RegisterIndex * y: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = or &lt;integer type&gt; &lt;x&gt; &lt;y&gt;</c>
        /// </para>
        /// <para>
        /// Converts the values in the <paramref name="x"/> and <paramref name="y"/> registers to the specified integer type,
        /// computes the bitwise <c>OR</c>, and returns the result.
        /// </para>
        /// </summary>
        | Or of PrimitiveType * x: RegisterIndex * y: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = not &lt;integer type&gt; &lt;value&gt;</c>
        /// </para>
        /// <para>
        /// Converts the value in the register to the specified integer type, computes the bitwise <c>NOT</c>, and returns the
        /// result.
        /// </para>
        /// </summary>
        | Not of PrimitiveType * RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = xor &lt;integer type&gt; &lt;x&gt; &lt;y&gt;</c>
        /// </para>
        /// <para>
        /// Converts the values in the <paramref name="x"/> and <paramref name="y"/> registers to the specified integer type,
        /// computes the bitwise <c>XOR</c>, and returns the result.
        /// </para>
        /// </summary>
        | Xor of PrimitiveType * x: RegisterIndex * y: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = rem [throw.div0] &lt;numeric type&gt; &lt;x&gt; &lt;y&gt;</c>
        /// </para>
        /// <para>
        /// Converts the values in the <paramref name="x"/> and <paramref name="y"/> registers to the specified integer type,
        /// divides the value in register <paramref name="x"/> by the value in register <paramref name="y"/>, and returns the
        /// remainder.
        /// </para>
        /// </summary>
        | Rem of ArithmeticFlags * PrimitiveType * x: RegisterIndex * y: RegisterIndex

        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = rotl &lt;numeric type&gt; &lt;value&gt;</c>
        /// </para>
        /// <para>
        /// Converts the in the <paramref name="i"/> register to the specified integer type, and shifts the value left by the
        /// specified integer <paramref name="amount"/>
        /// </para>
        /// </summary>
        | Rotl of PrimitiveType * amount: RegisterIndex * i: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = rotr &lt;integer type&gt; &lt;value&gt;</c>
        /// </para>
        /// <para>
        /// Converts the value in the <paramref name="i"/> register to the specified integer type, and shifts the value right by
        /// the specified integer <paramref name="amount"/>.
        /// </para>
        /// </summary>
        | Rotr of PrimitiveType * amount: RegisterIndex * i: RegisterIndex

        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = const.s &lt;numeric type&gt; &lt;value&gt;</c>
        /// </para>
        /// <para>
        /// Returns a signed integer of the specified numeric type.
        /// </para>
        /// </summary>
        /// <remarks>
        /// The integer is stored as a variable-width signed integer.
        /// </remarks>
        | Const_s of PrimitiveType * value: varint
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = const.u &lt;numeric type&gt; &lt;value&gt;</c>
        /// </para>
        /// <para>
        /// Returns an unsigned integer of the specified numeric type.
        /// </para>
        /// </summary>
        /// <remarks>
        /// The integer is stored as a variable-width unsigned integer.
        /// </remarks>
        | Const_u of PrimitiveType * value: uvarint
        | Const_f32 of value: single
        | Const_f64 of value: double
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = const.true &lt;numeric type&gt;</c>
        /// </para>
        /// <para>
        /// Returns a <see langword="true"/> value or the integer <c>1</c> as the specified numeric type.
        /// </para>
        /// </summary>
        | Const_true of PrimitiveType
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = const.false &lt;numeric type&gt;</c>
        /// </para>
        /// <para>
        /// An alias for <c>const.zero</c>, returns a <see langword="false"/> value or the integer <c>0</c> as the specified
        /// numeric type.
        /// </para>
        /// </summary>
        | Const_false of PrimitiveType
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = const.zero &lt;numeric type&gt;</c>
        /// </para>
        /// <para>
        /// Returns a value of <c>0</c> as the specified numeric type.
        /// </para>
        /// </summary>
        | Const_zero of PrimitiveType

        /// <summary>
        /// <para>
        /// <c>br &lt;target&gt;</c>
        /// </para>
        /// <para>
        /// Performs an unconditional branch to the block specified by the <paramref name="target"/> offset.
        /// </para>
        /// </summary>
        | Br of target: BlockOffset
        /// <summary>
        /// <para>
        /// <c>br.eq &lt;x&gt; &lt;y&gt; then &lt;btrue&gt; else &lt;bfalse&gt;</c>
        /// </para>
        /// <para>
        /// Branches to a block depending on if the value in register <paramref name="x"/> is equal to the value in register
        /// <paramref name="y"/>.
        /// </para>
        /// </summary>
        | Br_eq of x: RegisterIndex * y: RegisterIndex * btrue: BlockOffset * bfalse: BlockOffset
        /// <summary>
        /// <para>
        /// <c>br.ne &lt;x&gt; &lt;y&gt; then &lt;btrue&gt; else &lt;bfalse&gt;</c>
        /// </para>
        /// <para>
        /// Branches to a block depending on if the value in register <paramref name="x"/> is not equal to the value in register
        /// <paramref name="y"/>.
        /// </para>
        /// </summary>
        | Br_ne of x: RegisterIndex * y: RegisterIndex * btrue: BlockOffset * bfalse: BlockOffset
        /// <summary>
        /// <para>
        /// <c>br.lt &lt;x&gt; &lt;y&gt; then &lt;btrue&gt; else &lt;bfalse&gt;</c>
        /// </para>
        /// <para>
        /// Branches to a block depending on if the value in register <paramref name="x"/> is less than the value in register
        /// <paramref name="y"/>.
        /// </para>
        /// </summary>
        | Br_lt of x: RegisterIndex * y: RegisterIndex * btrue: BlockOffset * bfalse: BlockOffset
        /// <summary>
        /// <para>
        /// <c>br.gt &lt;x&gt; &lt;y&gt; then &lt;btrue&gt; else &lt;bfalse&gt;</c>
        /// </para>
        /// <para>
        /// Branches to a block depending on if the value in register <paramref name="x"/> is greater than the value in register
        /// <paramref name="y"/>.
        /// </para>
        /// </summary>
        | Br_gt of x: RegisterIndex * y: RegisterIndex * btrue: BlockOffset * bfalse: BlockOffset
        /// <summary>
        /// <para>
        /// <c>br.le &lt;x&gt; &lt;y&gt; then &lt;btrue&gt; else &lt;bfalse&gt;</c>
        /// </para>
        /// <para>
        /// Branches to a block depending on if the value in register <paramref name="x"/> is less than or equal to the value in
        /// register <paramref name="y"/>.
        /// </para>
        /// </summary>
        | Br_le of x: RegisterIndex * y: RegisterIndex * btrue: BlockOffset * bfalse: BlockOffset
        /// <summary>
        /// <para>
        /// <c>br.ge &lt;x&gt; &lt;y&gt; then &lt;btrue&gt; else &lt;bfalse&gt;</c>
        /// </para>
        /// <para>
        /// Branches to a block depending on if the value in the <paramref name="x"/> register is greater than or equal to the
        /// value in the <paramref name="y"/> register.
        /// </para>
        /// </summary>
        | Br_ge of x: RegisterIndex * y: RegisterIndex * btrue: BlockOffset * bfalse: BlockOffset
        /// <summary>
        /// <para>
        /// <c>br.true &lt;condition&gt; then &lt;btrue&gt; else &lt;bfalse&gt;</c>
        /// </para>
        /// <para>
        /// If the value in the <paramref name="condition"/> register is <see langword="true"/>, branches to the block specifie
        /// by the <paramref name="btrue"/> offset; otherwise, branches to the block specified by the <paramref name="bfalse"/>
        /// offset.
        /// </para>
        /// </summary>
        | Br_true of condition: RegisterIndex * btrue: BlockOffset * bfalse: BlockOffset
        /// <summary>
        /// <para>
        /// <c>mem.init [unaligned] [throw.invalid] &lt;count&gt; &lt;type&gt; at &lt;address&gt; with &lt;value&gt;</c>
        /// </para>
        /// <para>
        /// Sets the values at the address stored in the <paramref name="address"/> register to the specified initial value.
        /// </para>
        /// </summary>
        /// <remarks>
        /// <para>This instruction can be used, for example, to set the contents of a region of memory to all zeroes.</para>
        /// <para>Memory is not allocated by this instruction, use <c>alloca</c> or <c>alloca.obj</c> instead.</para>
        /// </remarks>
        | Mem_init of MemoryAccessFlags * count: RegisterIndex * TypeSignatureIndex * address: RegisterIndex * value: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>mem.st [unaligned] [throw.invalid] &lt;value&gt; &lt;type&gt; into &lt;address&gt;</c>
        /// </para>
        /// <para>
        /// Stores the value of the specified type into the memory specified by the <paramref name="address"/> register.
        /// </para>
        /// </summary>
        | Mem_st of MemoryAccessFlags * address: RegisterIndex * TypeSignatureIndex * value: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>mem.cpy [throw.invalid] &lt;count&gt; &lt;type&gt; from &lt;source&gt; to &lt;destination&gt;</c>
        /// </para>
        /// <para>
        /// Copies the specified number of items from the memory address specified by the <paramref name="source"/> register to
        /// the memory address specified by the <paramref name="destination"/> register.
        /// </para>
        /// </summary>
        | Mem_cpy of MemoryAccessFlags * count: RegisterIndex * TypeSignatureIndex * source: RegisterIndex * destination: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>mem.ld [unaligned] [throw.invalid] &lt;type&gt; from &lt;address&gt;</c>
        /// </para>
        /// <para>
        /// Loads a value of the specified type from the memory address specified by the <paramref name="address"/> register.
        /// </para>
        /// </summary>
        | Mem_ld of MemoryAccessFlags * TypeSignatureIndex * address: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = mem.init.const [unaligned] [throw.invalid] &lt;type&gt; at &lt;address&gt; with &lt;data&gt;</c>
        /// </para>
        /// <para>
        /// Sets the values at the address stored in the <paramref name="address"/> register to the specified module data.
        /// </para>
        /// </summary>
        | Mem_init_const of MemoryAccessFlags * TypeSignatureIndex * address: RegisterIndex * data: DataIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = obj.new &lt;constructor&gt; (&lt;argument1&gt;, &lt;argument2&gt;, ...)</c>
        /// </para>
        /// <para>
        /// Allocates a new object, calls the specified <paramref name="constructor"/> with the specified
        /// <paramref name="arguments"/>, and returns an object reference to the object.
        /// </para>
        /// </summary>
        | Obj_new of constructor: MethodIndex * arguments: vector<RegisterIndex>
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = obj.null</c>
        /// </para>
        /// <para>
        /// Returns a <see langword="null"/> object reference.
        /// </para>
        /// </summary>
        | Obj_null
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = obj.fd.ld &lt;field&gt; &lt;object&gt;</c>
        /// </para>
        /// <para>
        /// Returns the value of an object's field.
        /// </para>
        /// </summary>
        | Obj_fd_ld of field: FieldIndex * object: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>obj.fd.st &lt;field&gt; &lt;object&gt; &lt;source&gt;</c>
        /// </para>
        /// <para>
        /// Copies the value from the <paramref name="source"/> register into the field of the specified object.
        /// </para>
        /// </summary>
        | Obj_fd_st of field: FieldIndex * object: RegisterIndex * source: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = obj.fd.addr [throw.fail] &lt;field&gt; &lt;object&gt;</c>
        /// </para>
        /// <para>
        /// Returns a pointer to the field of the specified object.
        /// </para>
        /// </summary>
        | Obj_fd_addr of MemoryAccessFlags * field: FieldIndex * object: RegisterIndex
        /// <summary>
        /// <para>
        /// obj.throw &lt;ex&gt;
        /// </para>
        /// <para>
        /// Throws the exception stored in the <paramref name="ex"/> register.
        /// </para>
        /// </summary>
        | Obj_throw of ex: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = obj.arr.new &lt;etype&gt; &lt;length&gt;</c>
        /// </para>
        /// <para>
        /// Allocates a new array containing elements of the specified type with the specified <paramref name="length"/>, and
        /// returns an object reference to the array.
        /// </para>
        /// </summary>
        | Obj_arr_new of etype: TypeSignatureIndex * length: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = obj.arr.len [throw.ovf] &lt;numeric type&gt; &lt;array&gt;</c>
        /// </para>
        /// <para>
        /// Returns the length of the specified <paramref name="array"/>, as a number of the specified type.
        /// </para>
        /// </summary>
        | Obj_arr_len of ArithmeticFlags * PrimitiveType * array: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = obj.arr.get &lt;array&gt; &lt;index&gt;</c>
        /// </para>
        /// <para>
        /// Returns the item at the specified <paramref name="index"/> in the specified <paramref name="array"/>.
        /// </para>
        /// </summary>
        | Obj_arr_get of array: RegisterIndex * index: RegisterIndex
        //| Obj_arr_addr of array: RegisterIndex * index: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>obj.arr.set &lt;array&gt; &lt;index&gt; &lt;source&gt;</c>
        /// </para>
        /// <para>
        /// Copies the value in the <paramref name="source"/> register into the specified <paramref name="array"/> at the
        /// specified <paramref name="index"/>.
        /// </para>
        /// </summary>
        | Obj_arr_set of array: RegisterIndex * index: RegisterIndex * source: RegisterIndex // TODO: Have flags for array access.
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = obj.arr.addr [throw.oob] &lt;array&gt; &lt;index&gt; &lt;source&gt;</c>
        /// </para>
        /// <para>
        /// Returns a pointer to the array element at the specified <paramref name="index"/>
        /// </para>
        /// </summary>
        | Obj_arr_addr of MemoryAccessFlags * array: RegisterIndex * index: RegisterIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = obj.arr.const &lt;etype&gt; &lt;data&gt;</c>
        /// </para>
        /// <para>
        /// Allocates a new array containing elements of the specified type from the specified module <paramref name="data"/>,
        /// and returns an object reference to the array.
        /// </para>
        /// </summary>
        | Obj_arr_const of etype: TypeSignatureIndex * data: DataIndex

        // TODO: How to allocate an "array" of object references? Maybe have a separate RegisterType type?
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = alloca [throw.fail] &lt;count&gt; &lt;type&gt;</c>
        /// </para>
        /// <para>
        /// Allocates a contiguous region of memory enough to hold the specified number of instances of the specified types, and
        /// returns a pointer to the region of memory.
        /// </para>
        /// </summary>
        | Alloca of AllocationFlags * count: RegisterIndex * TypeSignatureIndex
        /// <summary>
        /// <para>
        /// <c>&lt;result&gt; = alloca.obj [throw.fail] &lt;constructor&gt; (&lt;argument1&gt;, &lt;argument2&gt;, ...)</c>
        /// </para>
        /// <para>
        /// Allocates a region of memory to contain an instance of a type, calls the specified <paramref name="constructor"/>
        /// providing a pointer to the region of memory along with the specified <paramref name="arguments"/>, and returns a
        /// pointer to the region of memory.
        /// </para>
        /// </summary>
        | Alloca_obj of AllocationFlags * constructor: MethodIndex * arguments: vector<RegisterIndex> // TODO: How to support VTable for some instances, add a new alloca instruction?

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
    | Virtual = 0b0000_0100uy
    | ValidMask = 0b0000_0111uy
    // TODO: Figure out if something analagous to C# "new" keyword is needed

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type MethodBody =
    /// Defined in the current module.
    | Defined of CodeIndex
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

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type MethodOverride =
    { /// Specifies the method to override.
      Declaration: MethodIndex
      /// Specifies the new implementation of the method, the method must be defined in the current type.
      Implementation: MethodIndex }

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
      VTable: vector<MethodOverride> }

[<NoComparison; NoEquality>]
type BlockExceptionHandler =
    { /// Specifies the local register that the exception object is stored into when an exception is thrown. If omitted, the
      /// exception object is ignored.
      ExceptionRegister: LocalIndex voption
      CatchBlock: CodeBlockIndex }

[<Flags>]
type CodeBlockFlags =
    | None = 0uy
    | ExceptionHandlerIgnoresException = 1uy
    | ExceptionHandlerStoresException = 2uy
    | ExceptionHandlingMask = 2uy

[<NoComparison; NoEquality>]
type CodeBlock =
    { /// Specifies the block that control should be transferred to if an exception is thrown inside this block.
      ExceptionHandler: BlockExceptionHandler voption
      /// Specifies which temporary registers in the current block map to which local register. Each local register must map to
      /// exactly one temporary register in exactly one block.
      Locals: vector<struct(TemporaryIndex * LocalIndex)>
      /// <remarks>
      /// Both the byte length and the actual number of instructions are included to simplify parsing.
      /// </remarks>
      Instructions: LengthEncoded<vector<InstructionSet.Instruction>> }

    member Flags : CodeBlockFlags

[<NoComparison; NoEquality>]
type Code =
    { /// Indicates the total number of local registers.
      LocalCount: uvarint
      Blocks: vector<CodeBlock> }

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
