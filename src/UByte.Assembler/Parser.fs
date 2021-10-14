module UByte.Assembler.Parser

open System
open System.Collections.Immutable

open FParsec

open UByte.Helpers

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

let integerlit = numberLiteral NumberLiteralOptions.DefaultInteger "integer literal" .>> whitespace

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

type ParsedVersionNumbers = Position * VersionNumbers

let vernums =
    let num: Parser<uvarint, _> =
        numberLiteral NumberLiteralOptions.None "version number" |>> fun n -> UInt32.Parse n.String
    whitespace >>. many (num .>> whitespace) |>> VersionNumbers.ofList

let namedecl dname pname = dname >>. pname .>> whitespace |> line
let namedecl' = namedecl (keyword "name") symbol
let verdecl name: Parser<ParsedVersionNumbers, _> = name >>. getPosition .>>. vernums |> line
let verdecl' = verdecl (keyword "version")

type ParsedTypeSignature = (Symbol -> TypeDefinitionIndex voption) -> Result<AnyType, Symbol>

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
    |>> fun prim _ -> ValueType.Primitive prim |> AnyType.ValueType |> Result.Ok
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
    | InvalidIntegerLiteral of Position * size: int32 * literal: string

type IInstructionResolver =
    abstract FindField: field: Symbol -> FieldIndex voption
    abstract FindMethod: method: Symbol -> MethodIndex voption

type RegisterLookup = Symbol -> RegisterIndex voption

type InstructionErrorsBuilder = System.Collections.Generic.ICollection<InvalidInstructionError>

type ParsedInstruction = RegisterLookup -> IInstructionResolver -> InstructionErrorsBuilder -> InstructionSet.Instruction voption

[<Struct>]
type ParsedCode = { Locals: ParsedCodeLocals list; Instructions: ParsedInstruction list }

let code: Parser<ParsedCode, _> =
    let registers =
        let register =
            period >>. keyword "local" >>. whitespace >>. symbol .>> whitespace |> line

        period >>. keyword "type" >>. pipe2
            (symbol .>> whitespace)
            (many register)
            (fun rtype regs -> { ParsedCodeLocals.LocalsType = rtype; LocalNames = regs })

    let inline addErrorTo (errors: InstructionErrorsBuilder) e =
        errors.Add e
        ValueNone

    let lookupRegisterName (lookup: RegisterLookup) (errors: InstructionErrorsBuilder) id: RegisterIndex voption =
        match lookup id with
        | ValueSome i -> ValueSome i
        | ValueNone -> addErrorTo errors (InvalidInstructionError.UndefinedRegister id)

    let lookupRegisterList lookup errors names =
        let rec inner (registers: ImmutableArray<RegisterIndex>.Builder) success names =
            match names with
            | [] -> if success then ValueSome(registers.ToImmutable()) else ValueNone
            | name :: remaining ->
                let success' =
                    match lookupRegisterName lookup errors name with
                    | ValueSome i ->
                        registers.Add i
                        success
                    | ValueNone ->
                        false

                inner registers success' remaining
        inner (ImmutableArray.CreateBuilder()) true names

    let lookupRegisterArray lookup names errors =
        let mutable resolved, success = Array.zeroCreate(Array.length names), true

        for i = 0 to resolved.Length - 1 do
            match lookupRegisterName lookup errors names.[i] with
            | ValueSome rindex -> resolved.[i] <- rindex
            | ValueNone -> success <- false

        if success then
            ValueSome(System.Runtime.CompilerServices.Unsafe.As<RegisterIndex[], ImmutableArray<RegisterIndex>> &resolved)
        else
            ValueNone

    let instruction: Parser<ParsedInstruction, _> = choice [
        // NOTE: For performance, can be replaced with a lookup table (https://www.quanttec.com/fparsec/users-guide/performance-optimizations.html#low-level-parser-implementations)
        //let instructions = Dictionary<string, Parser<ParsedInstruction, _>>()

        // TODO: Add parsing of other instructions (https://github.com/davnavr/ubyte/blob/9fcc545407852a2573f3eb1d9cb568f1b15de26e/src/UByte.Assembler/Assemble.fs#L403)

        let noop name instr = keyword name >>. preturn (fun _ _ _ -> ValueSome instr) |> line
        noop "nop" InstructionSet.Nop

        let withRegisterCount name count instr =
            keyword name >>. parray count symbol |>> fun registers rlookup _ errors -> voptional {
                let! registers' = lookupRegisterArray rlookup registers errors
                return instr registers'
            }

        let withThreeRegisters name instr =
            withRegisterCount name 3 (fun registers -> instr(registers.[0], registers.[1], registers.[2]))

        withThreeRegisters "add.ovf" InstructionSet.Add_ovf
        withThreeRegisters "add" InstructionSet.Add
        withThreeRegisters "sub.ovf" InstructionSet.Sub_ovf
        withThreeRegisters "sub" InstructionSet.Sub
        withThreeRegisters "mul.ovf" InstructionSet.Mul_ovf
        withThreeRegisters "mul" InstructionSet.Mul
        withThreeRegisters "div" InstructionSet.Div

        keyword "const.i32" >>. pipe3 getPosition integerlit symbol (fun pos value destination rlookup _ errors -> voptional {
            let! value' =
                match Int64.TryParse value.String with
                | true, i when i >= int64 Int32.MinValue && i <= int64 UInt32.MaxValue -> ValueSome(int32 i)
                | _ -> addErrorTo errors (InvalidInstructionError.InvalidIntegerLiteral(pos, sizeof<int32>, value.String))

            let! destination' = lookupRegisterName rlookup errors destination

            return InstructionSet.Const_i32(value', destination')
        })

        keyword "ret" >>. many symbol |> line |>> fun registers rlookup _ errors -> voptional {
            let! registers' = lookupRegisterList rlookup errors registers
            return InstructionSet.Ret registers'
        }
    ]

    pipe2
        (period >>. keyword "locals" >>. whitespace >>. block registers |> many)
        (instruction .>> whitespace |> line |> many)
        (fun locals instrs -> { ParsedCode.Locals = locals; Instructions = instrs })
    |> block

type ParsedNamespace = { NamespaceName: Symbol list }

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

[<RequireQualifiedAccess>]
type TypeDefAttr = | Visibility of Position * VisibilityFlags

let tdefattr =
    choice [
        getPosition .>>. visibility |>> TypeDefAttr.Visibility
    ]
    .>> whitespace
    |> many

[<RequireQualifiedAccess>]
type FieldDefAttr =
    | Visibility of Position * VisibilityFlags
    | Flag of Position * FieldFlags

let fdefattr =
    choice [
        getPosition .>>. visibility |>> FieldDefAttr.Visibility
        getPosition .>>. flags [
            "mutable", FieldFlags.Mutable
        ]
        |>> FieldDefAttr.Flag
    ]
    .>> whitespace
    |> many

[<RequireQualifiedAccess>]
type FieldDefDecl =
    | Type of Symbol
    | Name of Symbol

let fdefdecl =
    period >>. choice [
        namedecl' |>> FieldDefDecl.Name
        keyword "type" >>. symbol |>> FieldDefDecl.Type
    ]
    |> line
    |> many

[<RequireQualifiedAccess>]
type MethodDefAttr =
    | Visibility of Position * VisibilityFlags
    | Flag of Position * MethodFlags

let mdefattr =
    choice [
        getPosition .>>. visibility |>> MethodDefAttr.Visibility

        getPosition .>>. flags [
            "instance", MethodFlags.Instance
        ]
        |>> MethodDefAttr.Flag
    ]
    .>> whitespace
    |> many

[<RequireQualifiedAccess>]
type MethodDefDecl =
    | Signature of Symbol
    | Name of Symbol
    | Body of Position * ((Symbol -> CodeIndex voption) -> Result<MethodBody, Name>)

let mdefdecl =
    period >>. choice [
        namedecl' |>> MethodDefDecl.Name
        keyword "signature" >>. symbol |>> MethodDefDecl.Signature
        keyword "body" >>. choice [
            keyword "defined" >>. symbol |>> fun ((pos, coden) as body) ->
                MethodDefDecl.Body(pos, fun lookup ->
                    match lookup body with
                    | ValueSome codei -> Result.Ok(MethodBody.Defined codei)
                    | ValueNone -> Result.Error coden)
        ]
    ]
    |> line
    |> many

[<RequireQualifiedAccess>]
type TypeDefDecl =
    | Name of Symbol
    | Namespace of Symbol
    | Field of Symbol voption * FieldDefAttr list * FieldDefDecl list
    | Method of Symbol voption * MethodDefAttr list * MethodDefDecl list

let tdefdecl =
    period >>. choice [
        namedecl' |>> TypeDefDecl.Name
        namedecl (keyword "namespace") symbol |>> TypeDefDecl.Namespace
        keyword "field" >>. tuple3 (symbol |>> ValueSome) fdefattr fdefdecl |>> TypeDefDecl.Field
        keyword "method" >>. tuple3 (symbol |>> ValueSome) mdefattr mdefdecl |>> TypeDefDecl.Method
    ]
    |> line
    |> many

[<RequireQualifiedAccess>]
type ModuleImportDecl = | Name of Name | Version of ParsedVersionNumbers

[<RequireQualifiedAccess>]
type ParsedDeclaration =
    | Module of Position * Symbol voption * Name
    | FormatVersion of ParsedVersionNumbers
    | ModuleVersion of ParsedVersionNumbers
    | Identifier of Symbol * string
    | Signature of Symbol * ParsedSignature
    | ImportedModule of Symbol * ModuleImportDecl list
    | Code of Symbol * ParsedCode
    | Namespace of Symbol * ParsedNamespace
    | TypeDefinition of Symbol * TypeDefAttr list * TypeDefDecl list
    | EntryPoint of Symbol

let declarations: Parser<ParsedDeclaration list, unit> =
    let declaration = period >>. choice [
        keyword "module" >>. choice [
            let mdimport =
                period >>. choice [
                    namedecl (keyword "name") name |>> ModuleImportDecl.Name
                    verdecl' |>> ModuleImportDecl.Version
                ]
                |> line
                |> many

            keyword "extern" >>. symbol .>>. block mdimport |>> ParsedDeclaration.ImportedModule
            pipe2 getPosition name (fun pos name -> ParsedDeclaration.Module(pos, ValueNone, name)) |> line
        ]

        verdecl (keyword "format") |>> ParsedDeclaration.FormatVersion
        verdecl' |>> ParsedDeclaration.ModuleVersion
        keyword "identifier" >>. symbol .>>. identifier |>> ParsedDeclaration.Identifier |> line

        keyword "signature" >>. symbol .>>. choice [
            keyword "type" >>. typesig |>> ParsedSignature.Type
            keyword "method" >>. methodsig |>> ParsedSignature.Method
        ]
        |>> ParsedDeclaration.Signature
        |> line

        keyword "code" >>. symbol .>>. code |>> ParsedDeclaration.Code

        keyword "namespace" >>. pipe2 symbol (block (namedecl (declaration "name") (many symbol))) (fun id name ->
            ParsedDeclaration.Namespace(id, { ParsedNamespace.NamespaceName = name }))

        keyword "type" >>. tuple3 symbol tdefattr (block tdefdecl) |>> ParsedDeclaration.TypeDefinition
        keyword "entrypoint" >>. symbol |>> ParsedDeclaration.EntryPoint
    ]
    // TODO: If declaration could not be parsed, store error and parse the next one

    many (whitespace >>. declaration .>> whitespace) .>> eof
