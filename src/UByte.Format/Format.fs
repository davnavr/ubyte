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
type VersionNumbers = vector<uint8>

[<Struct; NoComparison; StructuralEquality>]
type UString =
    | UString of vector<Rune>

    override this.ToString() =
        let (UString cpoints) = this
        let sb = StringBuilder cpoints.Length
        for c in cpoints do sb.Append c |> ignore
        sb.ToString()

[<Struct; NoComparison; StructuralEquality>]
type Name =
    | Name of UString

    override this.ToString() = let (Name name) = this in name.ToString()

type File =
    { Magic: Magic
      Name: UString
      Version: VersionNumbers }

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
