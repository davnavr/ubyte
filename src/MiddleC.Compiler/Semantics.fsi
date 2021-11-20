namespace MiddleC.Compiler.Semantics

open System.Collections.Immutable

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type CheckedModule =
    { Errors: ImmutableArray<unit> }

[<RequireQualifiedAccess>]
module TypeChecker =
    val check : files: ImmutableArray<MiddleC.Compiler.Parser.ParsedFile> -> CheckedModule
