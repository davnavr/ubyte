namespace MiddleC.Compiler.Semantics

open System.Collections.Immutable

[<NoComparison; NoEquality>]
type SemanticErrorMessage =
    | DuplicateTypeDefinition of MiddleC.Compiler.Parser.TypeIdentifier
    | UnknownError of message: string

    override ToString : unit -> string

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type SemanticError =
    { Line: uint32
      Column: uint32
      Source: MiddleC.Compiler.Parser.ParsedFile
      Message: SemanticErrorMessage }

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type CheckedModule =
    { Errors: ImmutableArray<SemanticError> }

[<RequireQualifiedAccess>]
module TypeChecker =
    val check :
        files: ImmutableArray<MiddleC.Compiler.Parser.ParsedFile> ->
        imports: ImmutableArray<UByte.Resolver.ResolvedModule> -> CheckedModule
