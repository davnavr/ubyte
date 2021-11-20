namespace MiddleC.Compiler.Semantics

open System.Collections.Immutable

open UByte.Resolver

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type CheckedModule =
    { Errors: ImmutableArray<unit> }

[<RequireQualifiedAccess>]
module TypeChecker =
    val check :
        files: ImmutableArray<MiddleC.Compiler.Parser.ParsedFile> ->
        imports: ImmutableArray<ResolvedModule> -> CheckedModule
