namespace UByte.Format

open System.Collections.Immutable

open UByte.Format.Model

[<Struct; StructuralComparison; StructuralEquality>]
type CodeBuilderRegister =
    internal
    | CodeBuilderRegister of uint32

[<Sealed>]
type CodeBuilder =
    new : argumentRegisterCount: uint32 -> CodeBuilder

    member ArgumentAt : index: uint32 -> CodeBuilderRegister

    member Const_i : PrimitiveType * value: varint -> CodeBuilderRegister

    member Ret : values: ImmutableArray<CodeBuilderRegister> -> unit
