module UByte.Assembler.Parser

open FParsec

let whitespace =
    spaces
    .>> optional (attempt (skipChar ';') .>> skipRestOfLine true <?> "single-line comment")
    .>> spaces

type Atom =
    | Identifier of string
    | StringLiteral of string
    | Keyword of string
    | IntegerLiteral of int64
    //| FloatLiteral of float
    | NestedAtom of PositionedAtom list

    override this.ToString() =
        match this with
        | Identifier id -> sprintf "$%s" id
        | StringLiteral str -> sprintf "\"%s\"" str
        | Keyword word -> word
        | IntegerLiteral i -> string i
        | NestedAtom [] -> "()"
        | NestedAtom atoms ->
            System.Text.StringBuilder().Append('(').AppendJoin(' ', atoms).Append(')').ToString()

and PositionedAtom =
    { Atom: Atom
      Position: Position }

    override this.ToString() = this.Atom.ToString()

let keychar = choice [ asciiLetter; anyOf "_.<>" ]
let idchar = choice [ keychar; digit; ]

let (cell, cell') = createParserForwardedToRef<PositionedAtom list, unit>()

let atom: Parser<PositionedAtom, unit> =
    let quot = skipChar '\"'

    getPosition
    .>>. choiceL
        [
            skipChar '$' >>. (many1Chars idchar) |>> Identifier <?> "identifier"
            // TODO: Allow escape sequences
            quot >>. manyCharsTill idchar quot |>> StringLiteral <?> "string literal"
            keychar .>>. manyChars idchar |>> (fun (s, remaining) -> Keyword(string s + remaining)) <?> "keyword"
            pint64 |>> IntegerLiteral <?> "integer literal"
            // If parsing float literals, see http://www.quanttec.com/fparsec/reference/charparsers.html#members.numberLiteral to avoid problems with determining if a number if an integer or float
            cell |>> NestedAtom
        ]
        "atom"
    |>> fun (pos, atom) ->
        { Atom = atom; Position = pos }

do cell' := whitespace >>. between (skipChar '(' >>. whitespace) (skipChar ')') (many (atom .>> whitespace))

let sexpression = cell .>> whitespace .>> eof
