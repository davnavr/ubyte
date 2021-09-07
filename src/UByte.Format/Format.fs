module UByte.Format.Format

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text

[<RequireQualifiedAccess; Struct>]
type Magic = internal { Magic: ImmutableArray<byte> }

let (|Magic|) { Magic.Magic = magic } = magic

let mutable magic' = "ubyte"B

let magic = { Magic.Magic = Unsafe.As &magic' }

type vector<'T> = ImmutableArray<'T>

type uvarint = uint32

type varint = uint32

[<Struct; CustomComparison; CustomEquality>]
type VersionNumbers =
    | VersionNumbers of vector<uint32>

    override this.ToString() =
        let (VersionNumbers numbers) = this
        if numbers.IsDefaultOrEmpty
        then "0"
        else
            let sb = StringBuilder(2 * numbers.Length - 1)
            for i = 0 to numbers.Length - 1 do
                if i > 0 then sb.Append '.' |> ignore
                sb.Append numbers.[i] |> ignore
            sb.ToString()

    override this.GetHashCode() =
        let (VersionNumbers numbers) = this
        let mutable hcode = 0
        for i = 0 to numbers.Length - 1 do hcode <- hcode + int32 numbers.[i]
        hcode

    // TODO: If length differs, treat missing numbers as zeroes
    interface IEquatable<VersionNumbers> with
        member this.Equals(VersionNumbers other) =
            let (VersionNumbers this') = this
            Equality.lists this' other

    override this.Equals o =
        match o with
        | :? VersionNumbers as other -> this === other
        | _ -> false

    // TODO: If length differs, treat missing numbers as zeroes
    interface IComparable<VersionNumbers> with
        member this.CompareTo(VersionNumbers other) =
            let (VersionNumbers this') = this
            Comparison.lists this' other

    interface IComparable with member this.CompareTo o = Comparison.compare this (o :?> VersionNumbers)

let currentFormatVersion = ImmutableArray.Create(0u, 0u) |> VersionNumbers

let inline (|MatchesRune|) (c: char) (value: Rune) = Rune c = value

let escaped =
    let format = sprintf "\\u%04X"
    fun (sb: StringBuilder) ->
        function
        | MatchesRune '\n' true -> sb.Append "\\n"
        | MatchesRune '\r' true -> sb.Append "\\r"
        | MatchesRune '\\' true -> sb.Append "\\\\"
        | c -> format(uint32 c.Value) |> sb.Append

let inline (|RuneInRange|) (min: char) (max: char) (value: Rune) = Rune min <= value && Rune max >= value

[<Struct; NoComparison; StructuralEquality>]
type UString =
    | UString of vector<Rune>

    override this.ToString() =
        let (UString cpoints) = this
        let sb = StringBuilder cpoints.Length
        for c in cpoints do
            match c with
            | RuneInRange 'a' 'z' true
            | RuneInRange 'A' 'Z' true
            | RuneInRange '-' '9' true
            | MatchesRune '_' true
            | RuneInRange '\u00C0' '\u00F6' true -> sb.Append c
            | _ -> escaped sb c
            |> ignore
        sb.ToString()

[<Struct; NoComparison; StructuralEquality>]
type Name =
    | Name of UString

    override this.ToString() = let (Name name) = this in name.ToString()

type Section<'Contents> = 'Contents

type Module =
    { Magic: Magic
      FormatVersion: VersionNumbers
      Name: Name
      Version: VersionNumbers
      Data: Section<ImmutableArray<ImmutableArray<byte>>> }



module UString =
    let ofStr (s: string) =
        let chars = ImmutableArray.CreateBuilder s.Length
        let mutable enumerator = s.EnumerateRunes()
        while enumerator.MoveNext() do chars.Add enumerator.Current
        UString(chars.ToImmutable())

let (|Name|) (Name name) = name

module Name =
    let tryOfStr (UString str as name) =
        if str.IsDefaultOrEmpty
        then ValueNone
        else ValueSome(Name name)
