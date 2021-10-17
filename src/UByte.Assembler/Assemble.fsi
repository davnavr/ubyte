[<AutoOpen>]
module UByte.Assembler.Assemble

open FParsec

[<NoComparison; NoEquality>]
type AssemblerError

type AssemblerError with override ToString: unit -> string

val fromInput :
    input: (Parser<Parser.ParsedDeclaration list, unit> -> ParserResult<Parser.ParsedDeclaration list, unit>) ->
    Result<UByte.Format.Model.Module, System.Collections.Immutable.ImmutableArray<AssemblerError>>
