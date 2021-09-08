[<RequireQualifiedAccess>]
module UByte.Format.ValidateModule

[<NoComparison; NoEquality>]
type Validated

val (|Validated|) : Validated -> Format.Module

val validate : ``module``: Format.Module -> Validated

//val fromStream : source: Stream -> Format.Module
