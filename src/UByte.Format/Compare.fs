[<AutoOpen>]
module internal UByte.Format.Compare

open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
module Equality =
    let inline equals (x: 'X when 'X :> IEquatable<'Y>) (y: 'Y) = x.Equals y

    let private compareListItems (x: #IList<'X>) (y: #IList<'Y>) =
        let mutable eq, i = true, x.Count
        while eq && i < x.Count do
            eq <- equals x.[i] y.[i]
            i <- i + 1
        eq

    let lists (x: #IList<'X>) (y: #IList<'Y>) = x.Count = y.Count && compareListItems x y

    let spans (x: ReadOnlySpan<'X>) (y: ReadOnlySpan<'Y>) =
        let mutable eq, i = x.Length = y.Length, x.Length
        while eq && i < x.Length do
            eq <- equals x.[i] y.[i]
            i <- i + 1
        eq

let inline (===) (x: 'X) (y: 'Y) = Equality.equals x y

[<RequireQualifiedAccess>]
module Comparison =
    let inline compare (x: 'X when 'X :> IComparable<'Y>) (y: 'Y) = x.CompareTo y

    let private compareListItems (x: #IList<'X>) (y: #IList<'Y>) =
        let mutable eq, i = 0, x.Count
        while eq = 0 && i < x.Count do
            eq <- compare x.[i] y.[i]
            i <- i + 1
        eq

    let lists (x: #IList<'X>) (y: #IList<'Y>) =
        if x.Count > y.Count then 1
        elif y.Count < x.Count then -1
        else compareListItems x y
