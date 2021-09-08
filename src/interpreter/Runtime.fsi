[<RequireQualifiedAccess>]
module UByte.Interpreter.Runtime

open System.Collections.Generic
open System.IO

open UByte.Format

[<Sealed>]
type RuntimeModule =
    member Module: Model.Module

[<Sealed>]
type State =
    new: program: FileInfo (*-> IReadOnlyCollection<DirectoryInfo>*) -> State

val run: program: FileInfo (*-> IReadOnlyCollection<DirectoryInfo>*) -> unit
