[<AutoOpen>]
module UByte.Assembler.Assembler

type Position = FParsec.Position

[<NoComparison; NoEquality>]
type AssembleError

type AssembleError with override ToString: unit -> string

val assemble : Parser.PositionedAtom list -> Result<UByte.Format.Model.Module, AssembleError list>
