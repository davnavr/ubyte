[<RequireQualifiedAccess>]
module UByte.Assembler.Parser

open FParsec

type Atom =
    | Identifier of string
    | StringLiteral of string
    | Keyword of string
    | IntegerLiteral of int64
    //| FloatLiteral of float
    | NestedAtom of PositionedAtom list

    override ToString: unit -> string

and PositionedAtom =
    { Atom: Atom
      Position: Position }

    override ToString: unit -> string

val sexpression : Parser<PositionedAtom list, unit>
