module UByte.Assembler.Parser

open System.Collections.Immutable

open FParsec

open UByte.Format.Model

let whitespace =
    spaces
    .>> (skipString "//" .>> skipRestOfLine true <?> "single-line comment")
    .>> spaces

let propen = skipChar '('
let prclose = skipChar ')'
let bropen = skipChar '{'
let brclose = skipChar '}'

let separator = choice [
    notEmpty whitespace
    followedBy (choice [ bropen; brclose; propen; prclose ])
]

let keyword word = whitespace >>. skipString word .>> separator

let period = skipChar '.'

let idchars = [ asciiLetter; pchar '_' ]

let identifier =
    choiceL
        [
            choice [ asciiLetter; pchar '_' ] |> manyChars
            //between (skipChar '"')
        ]
        "name"
    .>> separator

let name = notEmpty identifier |>> Name.ofStr

let block contents = between bropen brclose contents

let line (inner: Parser<_, _>): Parser<_, _> =
    fun stream ->
        let start = stream.Position
        let reply = inner stream
        if reply.Status = ReplyStatus.Ok then
            let current = stream.Position
            if stream.IsEndOfStream || start.Line < current.Line then
                reply
            else
                Reply(ReplyStatus.Error, expected "newline")
        else
            reply

type Symbol = System.ValueTuple<Position, Name>

let symbol: Parser<Symbol, _> =
    pipe2
        getPosition
        (skipChar '@' >>. many1Chars (choice (digit :: idchars)))
        (fun pos id -> struct(pos, Name.ofStr id))
    .>> separator

[<RequireQualifiedAccess>]
type ParsedSignature =
    | Type of ((Symbol -> TypeDefinitionIndex voption) -> Result<AnyType, Name>)
    | Method of returnTypes: Symbol list * parameterTypes: Symbol list

[<Struct>]
type ParsedCodeLocals = { LocalsType: Symbol; LocalNames: Symbol list }

[<RequireQualifiedAccess>]
type InvalidInstructionError =
    | UndefinedRegister of Symbol
    | UndefinedMethod of Symbol

type IInstructionResolver =
    abstract FindField: field: Symbol -> FieldIndex voption
    abstract FindMethod: method: Symbol -> MethodIndex voption

type RegisterLookup = Symbol -> RegisterIndex voption

type InstructionErrorsBuilder = ImmutableArray<InvalidInstructionError>.Builder

type ParsedInstruction = RegisterLookup -> IInstructionResolver -> InstructionErrorsBuilder -> InstructionSet.Instruction voption

type ParsedCode = { Locals: ParsedCodeLocals; Instructions: ParsedInstruction list }

type ParsedNamespace = { NamespaceName: Symbol }

type ParsedFieldDefinition =
    { FieldVisibility: VisibilityFlags
      FieldFlags: FieldFlags
      FieldType: Symbol
      FieldName: Symbol voption }

type ParsedMethodDefinition =
    { MethodVisibility: VisibilityFlags
      MethodFlags: MethodFlags
      MethodBody: (Symbol -> CodeIndex voption) -> Result<MethodBody, Name>
      MethodSignature: Symbol
      MethodName: Symbol voption }

type ParsedTypeDefinition =
    { TypeVisibility: VisibilityFlags
      TypeName: Symbol voption
      TypeNamespace: Symbol voption
      DefinedFields: ParsedFieldDefinition list
      DefinedMethods: ParsedMethodDefinition list }

let vernums =
    let num: Parser<uvarint, _> =
        numberLiteral NumberLiteralOptions.None "version number" |>> fun n -> System.UInt32.Parse n.String
    whitespace >>. many (num .>> whitespace) |>> VersionNumbers.ofList

let namedecl = period >>. keyword "name" >>. name .>> whitespace |> line
let verdecl name = keyword name >>. getPosition .>>. vernums |> line

[<RequireQualifiedAccess>]
type ParsedDeclaration =
    | Module of Name
    | FormatVersion of Position * VersionNumbers
    | ModuleVersion of Position * VersionNumbers
    | Identifier of Symbol * string
    | Signature of Symbol * ParsedSignature
    | ImportedModule of Symbol * ModuleIdentifier
    | Code of Symbol * ParsedCode
    | Namespace of Symbol * ParsedNamespace
    | TypeDefinition of Symbol * ParsedTypeDefinition
    | EntryPoint of Symbol

let declarations: Parser<ParsedDeclaration list, _> =
    let declaration = period >>. choice [
        keyword "module" >>. choice [
            let mdimport = pipe2 namedecl (verdecl "version") <| fun name (_, ver) ->
                { ModuleIdentifier.ModuleName = name; Version = ver }

            keyword "extern" >>. symbol .>>. block mdimport |>> ParsedDeclaration.ImportedModule
            name .>> whitespace |>> ParsedDeclaration.Module |> line
        ]

        verdecl "format" |>> ParsedDeclaration.FormatVersion
        verdecl "version" |>> ParsedDeclaration.ModuleVersion
        keyword "identifier" >>. symbol .>>. identifier |>> ParsedDeclaration.Identifier |> line
    ]

    setUserState List.empty >>. many (whitespace >>. declaration .>> whitespace) .>> eof
