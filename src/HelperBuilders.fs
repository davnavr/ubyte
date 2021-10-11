[<AutoOpen>]
module internal UByte.Helpers.Builders

open System.Runtime.CompilerServices

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type ValueOptionBuilder =
    member inline _.Bind(value: 'T voption, body: 'T -> 'U voption) =
        match value with
        | ValueSome x -> body x
        | ValueNone -> ValueNone

    member inline _.Return(value: 'T) = ValueSome value

    member inline _.ReturnFrom(value: 'T voption) = value

let voptional = ValueOptionBuilder()
