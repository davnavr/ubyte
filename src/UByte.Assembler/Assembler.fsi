[<AutoOpen>]
module UByte.Assembler.Assembler

type Position = FParsec.Position

[<NoComparison; NoEquality>]
type AssemblerError

type AssemblerError with override ToString: unit -> string

val assemble : Parser.PositionedAtom list -> Result<UByte.Format.Model.Module, AssemblerError list>
