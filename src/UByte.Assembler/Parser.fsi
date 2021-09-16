[<RequireQualifiedAccess>]
module UByte.Assembler.Parser

open FParsec

[<NoComparison; StructuralEquality>]
type Atom =
    | Identifier of string
    | StringLiteral of string
    | Keyword of string
    | IntegerLiteral of int64
    //| FloatLiteral of float
    | NestedAtom of PositionedAtom list

    override ToString: unit -> string

    interface System.IEquatable<Atom>

and [<NoComparison; StructuralEquality>] PositionedAtom =
    { Atom: Atom
      Position: Position }

    override ToString: unit -> string

    interface System.IEquatable<PositionedAtom>

val sexpression : Parser<PositionedAtom list, unit>
