namespace MiddleC.Compiler.Semantics

open System.Collections.Immutable

open UByte.Resolver

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type CheckedModule =
    { Errors: ImmutableArray<string> }

[<RequireQualifiedAccess>]
module TypeChecker =
    val check :
        files: ImmutableArray<MiddleC.Compiler.Parser.ParsedFile> ->
        imports: ImmutableArray<ResolvedModule> -> CheckedModule
