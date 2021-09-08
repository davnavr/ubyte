[<RequireQualifiedAccess>]
module UByte.Format.ValidateModule

[<NoComparison; NoEquality>]
type Validated

val (|Validated|) : Validated -> Model.Module

val validate : ``module``: Model.Module -> Validated

//val fromStream : source: Stream -> Format.Module
