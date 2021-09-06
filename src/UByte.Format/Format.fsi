[<RequireQualifiedAccess>]
module UByte.Format.Format

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text

[<RequireQualifiedAccess; IsReadOnly; Struct>]
type Magic = internal { Magic: ImmutableArray<byte> }

val (|Magic|) : magic: Magic -> ImmutableArray<byte>

val magic : Magic

type vector<'T> = ImmutableArray<'T>

type VersionNumbers = vector<uint8>

/// Represents a length-encoded UTF-8 string.
[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type UString =
    | UString of vector<Rune>

    override ToString : unit -> string

    interface IEquatable<UString>

/// Represents a length-encoded UTF-8 string that cannot be empty.
[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type Name =
    internal
    | Name of UString

    override ToString: unit -> string

    interface IEquatable<Name>

val (|Name|) : name: Name -> UString

[<NoComparison; NoEquality>]
type File =
    { Magic: Magic
      Name: UString
      Version: VersionNumbers
       }

[<RequireQualifiedAccess>]
module UString =
    val ofStr : string -> UString

[<RequireQualifiedAccess>]
module Name =
    val tryOfStr : name: UString -> Name voption
