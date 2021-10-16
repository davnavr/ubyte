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
let comma = skipChar ','

let separator = choice [
    notEmpty whitespace
    followedBy (choice [ bropen; brclose; propen; prclose; comma ])
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
                let reply' = followedByNewline stream
                Reply(reply'.Status, reply.Result, reply'.Error)
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

let typesig: Parser<ParsedTypeSignature, _> =
    let tprimitive =
        choiceL
            [
                keyword "bool" >>. preturn PrimitiveType.Bool
                keyword "u8" >>. preturn PrimitiveType.U8
                keyword "s8" >>. preturn PrimitiveType.S8
                keyword "u16" >>. preturn PrimitiveType.U16
                keyword "s16" >>. preturn PrimitiveType.S16
                keyword "char16" >>. preturn PrimitiveType.Char16
                keyword "u32" >>. preturn PrimitiveType.U32
                keyword "s32" >>. preturn PrimitiveType.S32
                keyword "char32" >>. preturn PrimitiveType.Char32
                keyword "u64" >>. preturn PrimitiveType.U64
                keyword "s64" >>. preturn PrimitiveType.S64
                keyword "unative" >>. preturn PrimitiveType.UNative
                keyword "snative" >>. preturn PrimitiveType.SNative
                keyword "f32" >>. preturn PrimitiveType.F32
                keyword "f64" >>. preturn PrimitiveType.F64
            ]
            "primitive type"

    let treference =
        skipChar '&'  >>. choiceL
            [
                keyword "any" >>. preturn (fun _ -> Result.Ok ReferenceType.Any)
                keyword "def" >>. symbol |>> fun tname lookup ->
                    match lookup tname with
                    | ValueSome typei -> Result.Ok(ReferenceType.Defined typei)
                    | ValueNone -> Result.Error tname
                tprimitive |>> fun primt _ -> ValueType.Primitive primt |> ReferenceType.BoxedValueType |> Result.Ok
            ]
            "reference type"

    choice [
        tprimitive |>> fun prim _ -> ValueType.Primitive prim |> AnyType.ValueType |> Result.Ok
        treference |>> fun rt lookup -> Result.map AnyType.ReferenceType (rt lookup)
    ]

let methodsig: Parser<Symbol list * Symbol list, _> =
    let tlist = whitespace >>. propen >>. sepBy (whitespace >>. symbol) comma .>> prclose
    tlist .>>. tlist

[<Struct>]
type ParsedCodeLocals = { LocalsType: Symbol; LocalNames: Symbol list }

[<RequireQualifiedAccess>]
type InvalidInstructionError =
    | UnknownInstruction of Position * string
    | UndefinedRegister of Symbol
    | UndefinedField of Symbol
    | UndefinedMethod of Symbol
    | UndefinedTypeSignature of Symbol
    | InvalidIntegerLiteral of Position * size: int32 * literal: string
    | UndefinedLabel of Position * Name

type IInstructionResolver =
    abstract FindField: field: Symbol -> FieldIndex voption
    abstract FindMethod: method: Symbol -> MethodIndex voption
    abstract FindTypeSignature: signature: Symbol -> TypeSignatureIndex voption

type RegisterLookup = Symbol -> RegisterIndex voption

type InstructionErrorsBuilder = System.Collections.Generic.ICollection<InvalidInstructionError>

type CodeLabelLookup = (Position * Name) -> InstructionSet.InstructionOffset voption

[<RequireQualifiedAccess>]
type ParsedInstructionOrLabel =
    | Instruction of
        (RegisterLookup -> IInstructionResolver -> InstructionErrorsBuilder -> CodeLabelLookup -> InstructionSet.Instruction voption)
    | Label of Position * Name

type ParsedCode = { Locals: ParsedCodeLocals list; Arguments: Symbol list; Body: ParsedInstructionOrLabel list }

let code: Parser<ParsedCode, _> =
    let registers = between (propen .>> whitespace) prclose (sepBy1 (whitespace >>. symbol) comma)

    let inline addErrorTo (errors: InstructionErrorsBuilder) e =
        errors.Add e
        ValueNone

    let lookupRegisterName (lookup: RegisterLookup) errors id: RegisterIndex voption =
        match lookup id with
        | ValueSome _ as i -> i
        | ValueNone -> addErrorTo errors (InvalidInstructionError.UndefinedRegister id)

    let lookupMethodName (lookup: IInstructionResolver) errors id =
        match lookup.FindMethod id with
        | ValueSome _ as i -> i
        | ValueNone -> addErrorTo errors (InvalidInstructionError.UndefinedMethod id)

    let lookupFieldName (lookup: IInstructionResolver) errors id =
        match lookup.FindField id with
        | ValueSome _ as i -> i
        | ValueNone -> addErrorTo errors (InvalidInstructionError.UndefinedField id)

    let lookupTypeSignature (lookup: IInstructionResolver) errors id =
        match lookup.FindTypeSignature id with
        | ValueSome _ as i -> i
        | ValueNone -> addErrorTo errors (InvalidInstructionError.UndefinedTypeSignature id)

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

    let lookupCodeLabel (lookup: CodeLabelLookup) errors label =
        let i = lookup label
        if i.IsNone then addErrorTo errors (InvalidInstructionError.UndefinedLabel label) |> ignore
        i

    let instructionOrLabelName = many1Chars (choice [ asciiLetter; digit; pchar '.' ])
    let codeLabel = getPosition .>>. (instructionOrLabelName |>> Name.ofStr)

    let instruction: Parser<ParsedInstructionOrLabel, _> =
        let unknown pos name =
            ParsedInstructionOrLabel.Instruction(fun _ _ errors _ ->
                errors.Add(InvalidInstructionError.UnknownInstruction(pos, name))
                ValueNone)
            |> preturn

        let instructions = System.Collections.Generic.Dictionary()

        let addInstructionParser name parser = instructions.Add(name, parser |>> ParsedInstructionOrLabel.Instruction)

        addInstructionParser "nop" (preturn (fun _ _ _ _ -> ValueSome InstructionSet.Nop))

        let withOneRegister name instr =
            symbol |>> fun register rlookup _ errors _ -> voptional {
                let! register' = lookupRegisterName rlookup errors register
                return instr register'
            }
            |> addInstructionParser name

        withOneRegister "incr" InstructionSet.Incr
        withOneRegister "decr" InstructionSet.Decr
        withOneRegister "obj.null" InstructionSet.Obj_null

        let withRegisterCount name count instr =
            parray count symbol |>> fun registers rlookup _ errors _ -> voptional {
                let! registers' = lookupRegisterArray rlookup registers errors
                return instr registers'
            }
            |> addInstructionParser name

        let withTwoRegisters name instr =
            withRegisterCount name 2 (fun registers -> instr(registers.[0], registers.[1]))

        withTwoRegisters "obj.arr.len" InstructionSet.Obj_arr_len

        let withThreeRegisters name instr =
            withRegisterCount name 3 (fun registers -> instr(registers.[0], registers.[1], registers.[2]))

        withThreeRegisters "add.ovf" InstructionSet.Add_ovf
        withThreeRegisters "add" InstructionSet.Add
        withThreeRegisters "sub.ovf" InstructionSet.Sub_ovf
        withThreeRegisters "sub" InstructionSet.Sub
        withThreeRegisters "mul.ovf" InstructionSet.Mul_ovf
        withThreeRegisters "mul" InstructionSet.Mul
        withThreeRegisters "div" InstructionSet.Div
        withThreeRegisters "obj.arr.get" InstructionSet.Obj_arr_get
        withThreeRegisters "obj.arr.set" InstructionSet.Obj_arr_set

        let withConstantNumber name (number: Parser<NumberLiteral, _>) instr num =
            pipe3 getPosition number symbol <| fun pos value destination rlookup _ errors _ -> voptional {
                let! value' =
                    match num value with
                    | Result.Ok i -> ValueSome i
                    | Result.Error err -> addErrorTo errors (err pos)

                let! destination' = lookupRegisterName rlookup errors destination
                return instr(value', destination')
            }
            |> addInstructionParser name

        withConstantNumber "const.i32" integerlit InstructionSet.Const_i32 <| fun value ->
            match Int64.TryParse value.String with
            | true, i when i >= int64 Int32.MinValue && i <= int64 UInt32.MaxValue -> Result.Ok(int32 i)
            | _ -> Result.Error(fun pos -> InvalidInstructionError.InvalidIntegerLiteral(pos, sizeof<int32>, value.String))

        let objectFieldInstruction name instr =
            pipe3 symbol symbol symbol <| fun field oreg reg rlookup resolver errors _ -> voptional {
                let! field' = lookupFieldName resolver errors field
                let! oreg' = lookupRegisterName rlookup errors oreg
                let! reg' = lookupRegisterName rlookup errors reg
                return instr(field', oreg', reg')
            }
            |> addInstructionParser name

        objectFieldInstruction "obj.ldfd" InstructionSet.Obj_ldfd
        objectFieldInstruction "obj.stfd" InstructionSet.Obj_stfd

        let callInstructionRegisters = choice [
            between propen prclose (sepBy (whitespace >>. symbol) comma) .>> whitespace
            preturn List.empty
        ]
        let callLikeInstruction name instr =
            pipe3 symbol callInstructionRegisters callInstructionRegisters <| fun method rets args rlookup resolver errors _ ->
                voptional {
                    let! method' = lookupMethodName resolver errors method
                    let! returns = lookupRegisterList rlookup errors rets
                    let! arguments = lookupRegisterList rlookup errors args
                    return instr(method', arguments.ToImmutableArray(), returns.ToImmutableArray())
                }
            |> addInstructionParser name

        callLikeInstruction "call.virt" InstructionSet.Call_virt
        callLikeInstruction "call.ret" InstructionSet.Call_ret
        callLikeInstruction "call.virt.ret" InstructionSet.Call_virt_ret
        callLikeInstruction "call" InstructionSet.Call

        let comparisonBranchInstruction name instr =
            pipe3 symbol symbol codeLabel <| fun xreg yreg i rlookup _ errors labels -> voptional {
                let! xreg' = lookupRegisterName rlookup errors xreg
                let! yreg' = lookupRegisterName rlookup errors yreg
                let! i' = lookupCodeLabel labels errors i
                return instr(xreg', yreg', i')
            }
            |> addInstructionParser name

        comparisonBranchInstruction "br.eq" InstructionSet.Br_eq
        comparisonBranchInstruction "br.ne" InstructionSet.Br_ne
        comparisonBranchInstruction "br.lt" InstructionSet.Br_lt
        comparisonBranchInstruction "br.gt" InstructionSet.Br_gt
        comparisonBranchInstruction "br.le" InstructionSet.Br_le
        comparisonBranchInstruction "br.ge" InstructionSet.Br_ge

        let ifBranchInstruction name instr =
            pipe2 symbol codeLabel <| fun reg i rlookup _ errors labels -> voptional {
                let! reg' = lookupRegisterName rlookup errors reg
                let! i' = lookupCodeLabel labels errors i
                return instr(reg', i')
            }
            |> addInstructionParser name

        ifBranchInstruction "br.true" InstructionSet.Br_true
        ifBranchInstruction "br.false" InstructionSet.Br_false
        ifBranchInstruction "br.zero" InstructionSet.Br_false

        codeLabel |>> fun i _ _ errors labels -> voptional {
            let! i' = lookupCodeLabel labels errors i
            return InstructionSet.Br i'
        }
        |> addInstructionParser "br"

        many symbol |>> fun registers rlookup _ errors _ -> voptional {
            let! registers' = lookupRegisterList rlookup errors registers
            return InstructionSet.Ret registers'
        }
        |> addInstructionParser "ret"

        pipe3
            symbol
            symbol
            callInstructionRegisters
            (fun ctor ret args rlookup resolver errors _ -> voptional {
                let! constructor' = lookupMethodName resolver errors ctor
                let! result = lookupRegisterName rlookup errors ret
                let! arguments = lookupRegisterList rlookup errors args
                return InstructionSet.Obj_new(constructor', arguments.ToImmutableArray(), result)
            })
        |> addInstructionParser "obj.new"

        pipe3 symbol symbol symbol <| fun etype lreg rreg rlookup resolver errors _ -> voptional {
            let! etype' = lookupTypeSignature resolver errors etype
            let! lreg' = lookupRegisterName rlookup errors lreg
            let! rreg' = lookupRegisterName rlookup errors rreg
            return InstructionSet.Obj_arr_new(etype', lreg', rreg')
        }
        |> addInstructionParser "obj.arr.new"

        let label = skipChar ':' >>. whitespace

        getPosition .>>. instructionOrLabelName .>> whitespace >>= fun (pos, name) ->
            match instructions.TryGetValue name with
            | true, instr -> instr
            | false, _ -> choice [
                label >>. preturn(ParsedInstructionOrLabel.Label(pos, Name.ofStr name))
                unknown pos name .>> skipRestOfLine true
            ]

    let locals =
        period
        >>. keyword "locals"
        |> attempt
        >>. pipe2
            symbol
            (between (propen .>> whitespace) (prclose .>> whitespace) (sepBy1 (whitespace >>. symbol) comma))
            (fun rtype regs -> { ParsedCodeLocals.LocalsType = rtype; LocalNames = regs })

    let arguments = choice [
        period >>. keyword "arguments" >>. registers |> line
        preturn List.empty
    ]

    pipe3
        (many locals .>> whitespace)
        (arguments .>> whitespace)
        (instruction .>> whitespace |> line |> many)
        (fun locals arguments body -> { ParsedCode.Locals = locals; Arguments = arguments; Body = body })
    |> block

type ParsedNamespace = { NamespaceName: Symbol list }

let attributes (flags: (string * 'Flag) list when 'Flag :> System.Enum): Parser<_, _> =
    List.map (fun (name, flag) -> keyword name >>. preturn flag) flags |> choice

let visibility: Parser<VisibilityFlags, _> =
    attributes [
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
type FieldDefAttr = | Visibility of Position * VisibilityFlags | Flag of Position * FieldFlags

let fflags = getPosition .>>. attributes [
    "mutable", FieldFlags.Mutable
]

let fdefattr =
    choice [
        getPosition .>>. visibility |>> FieldDefAttr.Visibility
        fflags |>> FieldDefAttr.Flag
    ]
    .>> whitespace
    |> many

[<RequireQualifiedAccess>]
type FieldDefDecl = | Type of Symbol | Name of Symbol

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

let mflags = getPosition .>>. attributes [
    "instance", MethodFlags.Instance
    "constructor", MethodFlags.Constructor
]

let mdefattr =
    choice [
        getPosition .>>. visibility |>> MethodDefAttr.Visibility
        mflags |>> MethodDefAttr.Flag
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
        namedecl (keyword "namespace") symbol |>> TypeDefDecl.Namespace
        namedecl' |>> TypeDefDecl.Name
        keyword "field" >>. tuple3 (symbol |>> ValueSome) fdefattr (block fdefdecl) |>> TypeDefDecl.Field
        keyword "method" >>. tuple3 (symbol |>> ValueSome) mdefattr (block mdefdecl) |>> TypeDefDecl.Method
    ]
    |> line
    |> many

[<RequireQualifiedAccess>]
type FieldImportAttr = | Flag of Position * FieldFlags

let fimportattr =
    choice [
        // TODO: Avoid code duplication with fdefattr.
        fflags |>> FieldImportAttr.Flag
    ]
    .>> whitespace
    |> many

[<RequireQualifiedAccess>]
type FieldImportDecl = | Type of Symbol | Name of Symbol

let fimportdecl =
    period >>. choice [
        // TODO: Avoid code duplication with fdefdecl.
        namedecl' |>> FieldImportDecl.Name
        keyword "type" >>. symbol |>> FieldImportDecl.Type
    ]
    |> line
    |> many

[<RequireQualifiedAccess>]
type MethodImportAttr = | Flag of Position * MethodFlags

let mimportattr =
    choice [
        // TODO: Avoid code duplication with mdefattr.
        mflags |>> MethodImportAttr.Flag
    ]
    .>> whitespace
    |> many

[<RequireQualifiedAccess>]
type MethodImportDecl = | Signature of Symbol | Name of Symbol

let mimportdecl =
    period >>. choice [
        // TODO: Avoid code duplication with mdefdecl.
        namedecl' |>> MethodImportDecl.Name
        keyword "signature" >>. symbol |>> MethodImportDecl.Signature
    ]
    |> line
    |> many

[<RequireQualifiedAccess>]
type TypeImportDecl =
    | Module of Symbol
    | Name of Symbol
    | Namespace of Symbol
    | Field of Symbol * FieldImportAttr list * FieldImportDecl list
    | Method of Symbol * MethodImportAttr list * MethodImportDecl list

let timportdecl: Parser<TypeImportDecl list, _> =
    period >>. choice [
        // TODO: Avoid code duplication with tdefdecl.
        namedecl (keyword "module") symbol |>> TypeImportDecl.Module
        namedecl (keyword "namespace") symbol |>> TypeImportDecl.Namespace
        namedecl' |>> TypeImportDecl.Name
        keyword "field" >>. tuple3 symbol fimportattr (block fimportdecl) |>> TypeImportDecl.Field
        keyword "method" >>. tuple3 symbol mimportattr (block mimportdecl) |>> TypeImportDecl.Method
    ]
    |> line
    |> many

[<RequireQualifiedAccess>]
type ModuleImportDecl = | Name of Position * Name | Version of ParsedVersionNumbers

[<RequireQualifiedAccess>]
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

let declarations: Parser<ParsedDeclaration list, unit> =
    let declaration = period >>. choice [
        keyword "module" >>. choice [
            let mdimport =
                period >>. choice [
                    getPosition .>>. namedecl (keyword "name") name |>> ModuleImportDecl.Name
                    verdecl' |>> ModuleImportDecl.Version
                ]
                |> line
                |> many

            keyword "import" >>. symbol .>>. block mdimport |>> ParsedDeclaration.ImportedModule
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

        keyword "type" >>. choice [
            keyword "import" >>. tuple2 symbol (block timportdecl) |>> ParsedDeclaration.ImportedTypeDefinition

            tuple3 symbol tdefattr (block tdefdecl) |>> ParsedDeclaration.TypeDefinition
        ]

        keyword "entrypoint" >>. symbol |>> ParsedDeclaration.EntryPoint
    ]
    // TODO: If declaration could not be parsed, store error and parse the next one

    many (whitespace >>. declaration .>> whitespace) .>> eof
