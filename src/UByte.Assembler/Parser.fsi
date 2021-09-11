[<RequireQualifiedAccess>]
module UByte.Assembler.Parser

open FParsec

type Atom =
    | Identifier of string
    | StringLiteral of string
    | Keyword of string
    | IntegerLiteral of int64
    //| FloatLiteral of float
    | Nested of PositionedAtom list

and PositionedAtom =
    { Atom: Atom
      Position: Position }

val sexpression : Parser<PositionedAtom list, unit>
