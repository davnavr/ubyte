module UByte.Assembler.Parser

open FParsec

open UByte.Format.Model

type Symbol = System.ValueTuple<Position, Name>

type ParsedTypeSignature = (Symbol -> TypeDefinitionIndex voption) -> Result<AnyType, Symbol>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ParsedSignature =
    | Type of ParsedTypeSignature
    | Method of returnTypes: Symbol list * parameterTypes: Symbol list

type ParsedVersionNumbers = Position * VersionNumbers

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ModuleImportDecl =
    | Name of Position * Name
    | Version of ParsedVersionNumbers

[<System.Runtime.CompilerServices.IsReadOnly; Struct; NoComparison; NoEquality>]
type ParsedCodeLocals =
    { LocalsType: Symbol
      LocalNames: Symbol list }

[<RequireQualifiedAccess; NoComparison; NoEqualityAttribute>]
type InvalidInstructionError =
    | UnknownInstruction of Position * string
    | UndefinedRegister of Symbol
    | UndefinedField of Symbol
    | UndefinedMethod of Symbol
    | UndefinedTypeSignature of Symbol
    | InvalidIntegerLiteral of Position * size: int32 * literal: string

type IInstructionResolver =
    abstract FindField: field: Symbol -> FieldIndex voption
    abstract FindMethod: method: Symbol -> MethodIndex voption
    abstract FindTypeSignature: signature: Symbol -> TypeSignatureIndex voption

type RegisterLookup = Symbol -> RegisterIndex voption

type InstructionErrorsBuilder = System.Collections.Generic.ICollection<InvalidInstructionError>

type ParsedInstruction = RegisterLookup -> IInstructionResolver -> InstructionErrorsBuilder -> InstructionSet.Instruction voption

[<NoComparison; NoEquality>]
type ParsedCode =
    { Locals: ParsedCodeLocals list
      Arguments: Symbol list;
      Instructions: ParsedInstruction list }

[<NoComparison; NoEquality>]
type ParsedNamespace =
    { NamespaceName: Symbol list }

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type FieldDefAttr =
    | Visibility of Position * VisibilityFlags
    | Flag of Position * FieldFlags

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type FieldDefDecl =
    | Type of Symbol
    | Name of Symbol

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type MethodDefAttr =
    | Visibility of Position * VisibilityFlags
    | Flag of Position * MethodFlags

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type MethodDefDecl =
    | Signature of Symbol
    | Name of Symbol
    | Body of Position * ((Symbol -> CodeIndex voption) -> Result<MethodBody, Name>)

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type TypeDefAttr =
    | Visibility of Position * VisibilityFlags
    //| Extends of Symbol

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type TypeDefDecl =
    | Name of Symbol
    | Namespace of Symbol
    | Field of Symbol voption * FieldDefAttr list * FieldDefDecl list
    | Method of Symbol voption * MethodDefAttr list * MethodDefDecl list

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type FieldImportAttr =
    | Flag of Position * FieldFlags

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type FieldImportDecl =
    | Type of Symbol
    | Name of Symbol

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type MethodImportAttr =
    | Flag of Position * MethodFlags

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type MethodImportDecl =
    | Signature of Symbol
    | Name of Symbol

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type TypeImportDecl =
    | Module of Symbol
    | Name of Symbol
    | Namespace of Symbol
    | Field of Symbol * FieldImportAttr list * FieldImportDecl list
    | Method of Symbol * MethodImportAttr list * MethodImportDecl list

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ParsedDeclaration =
    | Module of Position * Symbol voption * Name
    | FormatVersion of ParsedVersionNumbers
    | ModuleVersion of ParsedVersionNumbers
    | Identifier of Symbol * string
    | Signature of Symbol * ParsedSignature
    | ImportedModule of Symbol * ModuleImportDecl list
    | ImportedTypeDefinition of Symbol * TypeImportDecl list
    | Code of Symbol * ParsedCode
    | Namespace of Symbol * ParsedNamespace
    | TypeDefinition of Symbol * TypeDefAttr list * TypeDefDecl list
    | EntryPoint of Symbol

val declarations : Parser<ParsedDeclaration list, unit>
