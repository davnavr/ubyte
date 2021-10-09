module UByte.Assembler.Parser

open System.Collections.Immutable

open FParsec

open UByte.Format.Model

let whitespace =
    spaces
    .>> optional (skipString "//" .>> skipRestOfLine true <?> "single-line comment")
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

let declaration word = period >>. keyword word

let idchars = [ asciiLetter; pchar '_' ]

let identifier =
    whitespace
    >>. choiceL
        [
            choice [ asciiLetter; pchar '_' ] |> manyChars
            //between (skipChar '"')
        ]
        "name"
    .>> separator

let name = notEmpty identifier |>> Name.ofStr

let block contents = between (whitespace >>. bropen .>> whitespace) (whitespace >>. brclose .>> whitespace) contents

let line (inner: Parser<_, _>): Parser<_, _> =
    fun stream ->
        let start = stream.Position
        let reply = inner stream
        if reply.Status = ReplyStatus.Ok then
            let current = stream.Position
            if stream.IsEndOfStream || start.Line < current.Line then
                reply
            else
                let reply = followedByNewline stream
                Reply(reply.Status, reply.Error)
        else
            reply

type Symbol = System.ValueTuple<Position, Name>

let symbol: Parser<Symbol, _> =
    pipe2
        getPosition
        (skipChar '@' >>. many1Chars (choice (digit :: idchars)))
        (fun pos id -> struct(pos, Name.ofStr id))
    .>> separator

let vernums =
    let num: Parser<uvarint, _> =
        numberLiteral NumberLiteralOptions.None "version number" |>> fun n -> System.UInt32.Parse n.String
    whitespace >>. many (num .>> whitespace) |>> VersionNumbers.ofList

let namedecl dname pname = declaration dname >>. pname .>> whitespace |> line
let namedecl' = namedecl "name" symbol
let verdecl name = name >>. getPosition .>>. vernums |> line

type ParsedTypeSignature = (Symbol -> TypeDefinitionIndex voption) -> Result<AnyType, Name>

[<RequireQualifiedAccess>]
type ParsedSignature =
    | Type of ParsedTypeSignature
    | Method of returnTypes: Symbol list * parameterTypes: Symbol list

let typesig: Parser<ParsedTypeSignature, _> = choice [
    choiceL
        [
            skipString "bool" >>. preturn PrimitiveType.Bool
            skipString "u8" >>. preturn PrimitiveType.U8
            skipString "s8" >>. preturn PrimitiveType.S8
            skipString "u16" >>. preturn PrimitiveType.U16
            skipString "s16" >>. preturn PrimitiveType.S16
            skipString "char16" >>. preturn PrimitiveType.Char16
            skipString "u32" >>. preturn PrimitiveType.U32
            skipString "s32" >>. preturn PrimitiveType.S32
            skipString "char32" >>. preturn PrimitiveType.Char32
            skipString "u64" >>. preturn PrimitiveType.U64
            skipString "s64" >>. preturn PrimitiveType.S64
            skipString "unative" >>. preturn PrimitiveType.UNative
            skipString "snative" >>. preturn PrimitiveType.SNative
            skipString "f32" >>. preturn PrimitiveType.F32
            skipString "f64" >>. preturn PrimitiveType.F64
        ]
        "primitive type"
    |>> fun prim -> fun _ -> ValueType.Primitive prim |> AnyType.ValueType |> Result.Ok
]

let methodsig: Parser<Symbol list * Symbol list, _> =
    let tlist = whitespace >>. propen >>. sepBy (whitespace >>. symbol .>> whitespace) (skipChar ',') .>> prclose
    tlist .>>. tlist

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

let code: Parser<ParsedCode, _> =
    let registers =
        let register =
            period >>. keyword "local" >>. whitespace >>. symbol .>> whitespace |> line

        period >>. keyword "type" >>. pipe2
            (symbol .>> whitespace)
            (many register)
            (fun rtype regs -> { ParsedCodeLocals.LocalsType = rtype; LocalNames = regs })

    let instruction: Parser<ParsedInstruction, _> = choice [
        // TODO: Add back parsing of other instructions (https://github.com/davnavr/ubyte/blob/9fcc545407852a2573f3eb1d9cb568f1b15de26e/src/UByte.Assembler/Assemble.fs#L403)

        let noop name instr = keyword name >>. preturn (fun _ _ _ -> ValueSome instr)
        noop "ret" (InstructionSet.Ret ImmutableArray.Empty)
        noop "nop" InstructionSet.Nop
    ]

    pipe2
        (period >>. keyword "locals" >>. whitespace >>. block registers)
        (instruction .>> whitespace |> line |> many)
        (fun locals instrs -> { ParsedCode.Locals = locals; Instructions = instrs })
    |> block

type ParsedNamespace = { NamespaceName: Symbol }

let inline attributes (flags: (string * 'Flag) list when 'Flag :> System.Enum): Parser<_, _> =
    List.map (fun (name, flag) -> keyword name >>. preturn flag) flags
    |> choice
    |> many
    |>> List.fold (fun flags flag -> flags ||| flag) Unchecked.defaultof<_>

let inline flags flags =
    choice [
        attributes flags |> attempt
        preturn Unchecked.defaultof<_>
    ]

let visibility: Parser<VisibilityFlags, _> =
    flags [
        "public", VisibilityFlags.Public
        "private", VisibilityFlags.Private
    ]

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

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ParsedFieldOrMethod =
    | ParsedField of ParsedFieldDefinition
    | ParsedMethod of ParsedMethodDefinition

let tdef: Parser<VisibilityFlags -> ParsedTypeDefinition, _> =
    //let inherited: Parser<_ list, _> =

    let members = period >>. choice [
        //keyword "field"

        let mflags = flags [
            "instance", MethodFlags.Instance
            "constructor", MethodFlags.Constructor
        ]

        let mbody = choice [
            keyword "defined" >>. symbol |>> fun body -> fun lookup ->
                let struct(_, name) = body
                match lookup body with
                | ValueSome codei -> Result.Ok(MethodBody.Defined codei)
                | ValueNone -> Result.Error name
        ]

        let mdecls =
            line (declaration "signature" >>. symbol)
            .>>. (line (declaration ".name" >>. symbol) |>> ValueSome <|> preturn ValueNone)

        keyword "method" >>. pipe5 symbol visibility mflags mbody (block mdecls) (fun id vis attrs mkind (signature, name) ->
            { ParsedMethodDefinition.MethodVisibility = vis
              MethodFlags = attrs
              MethodBody = mkind
              MethodSignature = signature
              MethodName = name }
            |> ParsedFieldOrMethod.ParsedMethod)
    ]

    pipe4
        namedecl'
        (namedecl "namespace" symbol)
        pzero //inherited
        (many members)
        (fun name ns () members -> fun vis ->
            { ParsedTypeDefinition.TypeVisibility = vis
              TypeName = ValueSome name
              TypeNamespace = ValueSome ns
              DefinedFields = // TODO: Simplify parsing of inner declarations in types.
                List.choose
                    (function
                    | ParsedFieldOrMethod.ParsedField field -> Some field
                    | _ -> None)
                    members
              DefinedMethods =
                List.choose
                    (function
                    | ParsedFieldOrMethod.ParsedMethod method -> Some method
                    | _ -> None)
                    members } )

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

let declarations: Parser<ParsedDeclaration list, unit> =
    let declaration = period >>. choice [
        keyword "module" >>. choice [
            let mdimport =
                pipe2 (namedecl "name" name) (verdecl (declaration "version")) <| fun name (_, ver) ->
                    { ModuleIdentifier.ModuleName = name; Version = ver }

            keyword "extern" >>. symbol .>>. block mdimport |>> ParsedDeclaration.ImportedModule
            name |>> ParsedDeclaration.Module |> line
        ]

        verdecl (keyword "format") |>> ParsedDeclaration.FormatVersion
        verdecl (keyword "version") |>> ParsedDeclaration.ModuleVersion
        keyword "identifier" >>. symbol .>>. identifier |>> ParsedDeclaration.Identifier |> line

        keyword "signature" >>. symbol .>>. choice [
            keyword "type" >>. typesig |>> ParsedSignature.Type
            keyword "method" >>. methodsig |>> ParsedSignature.Method
        ]
        |>> ParsedDeclaration.Signature
        |> line

        keyword "code" >>. symbol .>>. code |>> ParsedDeclaration.Code

        keyword "namespace" >>. pipe2 symbol (block namedecl') (fun id name ->
            ParsedDeclaration.Namespace(id, { ParsedNamespace.NamespaceName = name }))

        keyword "type" >>. pipe3 symbol visibility (block tdef) (fun id vis tkind ->
            ParsedDeclaration.TypeDefinition(id, tkind vis))

        keyword "entrypoint" >>. symbol |>> ParsedDeclaration.EntryPoint
    ]
    // TODO: If declaration could not be parsed, store error and parse the next one

    many (whitespace >>. declaration .>> whitespace) .>> eof
