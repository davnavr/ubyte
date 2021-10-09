module UByte.Assembler.Parser

open System.Collections.Immutable

open FParsec

open UByte.Format.Model

type Symbol = System.ValueTuple<Position, Name>

type ParsedTypeSignature = (Symbol -> TypeDefinitionIndex voption) -> Result<AnyType, Name>

[<RequireQualifiedAccess>]
type ParsedSignature =
    | Type of ParsedTypeSignature
    | Method of returnTypes: Symbol list * parameterTypes: Symbol list

[<System.Runtime.CompilerServices.IsReadOnly; Struct; NoComparison; NoEquality>]
type ParsedCodeLocals =
    { LocalsType: Symbol
      LocalNames: Symbol list }

[<RequireQualifiedAccess; NoComparison; NoEqualityAttribute>]
type InvalidInstructionError =
    | UndefinedRegister of Symbol
    | UndefinedMethod of Symbol

type IInstructionResolver =
    abstract FindField: field: Symbol -> FieldIndex voption
    abstract FindMethod: method: Symbol -> MethodIndex voption

type RegisterLookup = Symbol -> RegisterIndex voption

type InstructionErrorsBuilder = ImmutableArray<InvalidInstructionError>.Builder

type ParsedInstruction = RegisterLookup -> IInstructionResolver -> InstructionErrorsBuilder -> InstructionSet.Instruction voption

[<NoComparison; NoEquality>]
type ParsedCode =
    { Locals: ParsedCodeLocals
      Instructions: ParsedInstruction list }

[<NoComparison; NoEquality>]
type ParsedNamespace =
    { NamespaceName: Symbol }

[<NoComparison; NoEquality>]
type ParsedFieldDefinition =
    { FieldVisibility: VisibilityFlags
      FieldFlags: FieldFlags
      FieldType: Symbol
      FieldName: Symbol voption }

[<NoComparison; NoEquality>]
type ParsedMethodDefinition =
    { MethodVisibility: VisibilityFlags
      MethodFlags: MethodFlags
      MethodBody: (Symbol -> CodeIndex voption) -> Result<MethodBody, Name>
      MethodSignature: Symbol
      MethodName: Symbol voption }

[<NoComparison; NoEquality>]
type ParsedTypeDefinition =
    { TypeVisibility: VisibilityFlags
      TypeName: Symbol voption
      TypeNamespace: Symbol voption
      DefinedFields: ParsedFieldDefinition list
      DefinedMethods: ParsedMethodDefinition list }

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ParsedDeclaration =
    | Module of (*Symbol * *) Name // TODO: Allow refering to the current module
    | FormatVersion of Position * VersionNumbers
    | ModuleVersion of Position * VersionNumbers
    | Identifier of Symbol * string
    | Signature of Symbol * ParsedSignature
    | ImportedModule of Symbol * ModuleIdentifier
    | Code of Symbol * ParsedCode
    | Namespace of Symbol * ParsedNamespace
    | TypeDefinition of Symbol * ParsedTypeDefinition
    | EntryPoint of Symbol

val declarations : Parser<ParsedDeclaration list, unit> //Parser<ParsedDeclaration list, ParserError list>
