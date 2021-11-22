namespace UByte.Format

open System.Collections.Immutable
open System.Runtime.CompilerServices

open UByte.Format.Model

[<IsReadOnly; Struct>]
type CodeBuilderRegister = internal CodeBuilderRegister of uint32

[<Sealed>]
type CodeBlockBuilder () =
    member val Instructions = ImmutableArray.CreateBuilder<InstructionSet.Instruction>()
    //member val LocalRegisters = ImmutableArray.CreateBuilder<struct(TemporaryIndex * LocalIndex)>()

[<Sealed>]
type CodeBuilder (argumentRegisterCount: uint32) =
    let blocks = ImmutableArray.CreateBuilder<CodeBlockBuilder>()
    do blocks.Add(CodeBlockBuilder())
    let registers = ImmutableArray.CreateBuilder<struct(CodeBlockIndex * TemporaryIndex)>()
    let mutable nextRegisterIndex = argumentRegisterCount //let locals = Dictionary
    let mutable nextLocalIndex = 0u

    member _.ArgumentAt index =
        if index >= argumentRegisterCount then raise(System.IndexOutOfRangeException())
        CodeBuilderRegister index

    member _.CurrentBlockIndex = CodeBlockIndex.Index(uint32 blocks.Count - 1u)

    member this.RegisterIndexOf (CodeBuilderRegister index) =
        if index < argumentRegisterCount then
            index
        else
            let struct(blocki, Index tempi) = registers.[Checked.int32 index]
            if blocki = this.CurrentBlockIndex then
                // Note that since local registers are refered to with the last indices, building up of registers like this is allowed.
                argumentRegisterCount + nextLocalIndex + tempi
            else
                failwith "TODO: Do a local register lookup"
        |> RegisterIndex.Index

    member _.AddRegister() =
        let register = CodeBuilderRegister nextLocalIndex
        nextLocalIndex <- nextLocalIndex + 1u
        register

    member this.CurrentBlock = blocks.[Index.toInt this.CurrentBlockIndex]

    member this.Const_i(ty, value) =
        this.CurrentBlock.Instructions.Add(InstructionSet.Const_i(ty, value))
        this.AddRegister()

    member this.Ret(values: ImmutableArray<_>) =
        let mutable indices = Array.zeroCreate values.Length
        for i = 0 to indices.Length - 1 do indices.[i] <- this.RegisterIndexOf values.[i]
        this.CurrentBlock.Instructions.Add(InstructionSet.Ret(Unsafe.As<RegisterIndex[], ImmutableArray<RegisterIndex>> &indices))
