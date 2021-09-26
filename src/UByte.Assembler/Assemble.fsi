[<AutoOpen>]
module UByte.Assembler.Assemble

open FParsec

open UByte.Format.Model

[<NoComparison; NoEquality>]
type AssemblerError

type AssemblerError with override ToString: unit -> string

[<NoComparison; NoEquality>]
type IncompleteModule

[<NoComparison; NoEquality; Struct; System.Runtime.CompilerServices.IsReadOnly>]
type State =
    { Module: IncompleteModule
      Errors: AssemblerError list }

type AssemblerResult = Result<Module, AssemblerError list>

val fromInput : input: (Parser<AssemblerResult, State> -> State -> ParserResult<AssemblerResult, State>) -> AssemblerResult
