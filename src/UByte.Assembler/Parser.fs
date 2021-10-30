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
let sbopen = skipChar '['

let commaSepInsideParens inner =
    between (whitespace >>. propen .>> whitespace) (prclose .>> whitespace) (sepBy (whitespace >>. inner) comma)

let separator = choice [
    notEmpty whitespace
    followedByL (choice [ bropen; brclose; propen; prclose; comma; sbopen ]) "opening or closing"
]

let keyword word = whitespace >>. skipString word .>> separator

let period = skipChar '.'

let declaration word = period >>. keyword word

let idchars = [ asciiLetter; pchar '_' ]

// TODO: Handle escape sequences
let stringlit =
    let quote = skipChar '"'
    noneOf "\n\"\t\\"
    |> manyChars
    |> between quote quote
    <?> "string literal"

let identifier =
    whitespace
    >>. choiceL
        [
            choice idchars |> manyChars
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

let nsymbol c: Parser<Symbol, _> =
    pipe2
        getPosition
        (skipChar c >>. many1Chars (choice (digit :: idchars)))
        (fun pos id -> struct(pos, Name.ofStr id))
    .>> separator

let symbol: Parser<Symbol, _> = nsymbol '@'

let optsymbol c: Parser<Symbol voption, _> = choice [
    nsymbol c |>> ValueSome
    skipChar '_' >>% ValueNone
]

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

let tprimitive: Parser<_, _> =
    choiceL
        [
            keyword "bool" >>% PrimitiveType.Bool
            keyword "u8" >>% PrimitiveType.U8
            keyword "s8" >>% PrimitiveType.S8
            keyword "u16" >>% PrimitiveType.U16
            keyword "s16" >>% PrimitiveType.S16
            keyword "char16" >>% PrimitiveType.Char16
            keyword "u32" >>% PrimitiveType.U32
            keyword "s32" >>% PrimitiveType.S32
            keyword "char32" >>% PrimitiveType.Char32
            keyword "u64" >>% PrimitiveType.U64
            keyword "s64" >>% PrimitiveType.S64
            keyword "unative" >>% PrimitiveType.UNative
            keyword "snative" >>% PrimitiveType.SNative
            keyword "f32" >>% PrimitiveType.F32
            keyword "f64" >>% PrimitiveType.F64
        ]
        "primitive type"

let typesig: Parser<ParsedTypeSignature, _> =
    let tdefined name defined =
        keyword name >>. symbol |>> fun tname lookup ->
            match lookup tname with
            | ValueSome typei -> Result.Ok(defined typei)
            | ValueNone -> Result.Error tname

    let withTypeModifiers pmodifiers (ptype: Parser<_ -> Result<'Type, _>, _>) =
        pipe2 ptype (choice pmodifiers |> many) <| fun t modifiers ->
            List.fold
                (fun current modifier lookup -> current lookup |> modifier)
                t
                modifiers

    let tvalue =
        choiceL
            [
                tprimitive |>> fun prim _ -> Result.Ok(ValueType.Primitive prim)
                tdefined "struct" ValueType.Defined
            ]
            "value type"
        |> withTypeModifiers [
            skipChar '*' >>% Result.map ValueType.UnsafePointer
        ]

    let treference =
        choiceL
            [
                keyword "any" >>% fun _ -> Result.Ok ReferenceType.Any
                tdefined "class" ReferenceType.Defined
                keyword "boxed" >>. tprimitive |>> fun primt _ ->
                    ValueType.Primitive primt |> ReferenceType.BoxedValueType |> Result.Ok
            ]
            "reference type"

    let inline mapTypeParser mapping parser = parser |>> fun t lookup -> Result.map mapping (t lookup)

    let tsafe =
        choice [
            mapTypeParser ReferenceOrValueType.Reference treference
            mapTypeParser ReferenceOrValueType.Value tvalue
        ]
        |> withTypeModifiers [
            skipString "[]" >>% Result.map (ReferenceType.Vector >> ReferenceOrValueType.Reference)
        ]

    mapTypeParser
        (function
        | ReferenceOrValueType.Reference rt -> AnyType.ReferenceType rt
        | ReferenceOrValueType.Value vt -> AnyType.ValueType vt)
        tsafe
    //|> withTypeModifiers [
    //    skipChar '&' >>% Result.map AnyType.SafePointer
    //]

let methodsig: Parser<Symbol list * Symbol list, _> =
    let tlist = commaSepInsideParens symbol
    tlist .>>. tlist

let attributes (flags: (string * 'Flag) list when 'Flag :> System.Enum): Parser<_, _> =
    List.map (fun (name, flag) -> keyword name >>% flag) flags |> choice

let optattributes (flags: (_ * 'Flag) list): Parser<_, _> = attributes flags <|> preturn Unchecked.defaultof<'Flag>

[<RequireQualifiedAccess>]
type InvalidInstructionError =
    | UnknownInstruction of Position * string
    | UndefinedRegister of Symbol
    | UndefinedField of Symbol
    | UndefinedMethod of Symbol
    | UndefinedTypeSignature of Symbol
    | UndefinedData of Symbol
    | InvalidIntegerLiteral of Position * size: int32 * literal: string
    | UndefinedBlock of Symbol

type IInstructionResolver =
    abstract FindField: field: Symbol -> FieldIndex voption
    abstract FindMethod: method: Symbol -> MethodIndex voption
    abstract FindTypeSignature: signature: Symbol -> TypeSignatureIndex voption
    abstract FindData: signature: Symbol -> DataIndex voption

[<Struct>]
type ParsedRegister = { IsTemporary: bool; Name: Symbol }

type RegisterLookup = ParsedRegister -> RegisterIndex voption

type InstructionErrorsBuilder = System.Collections.Generic.ICollection<InvalidInstructionError>

type CodeBlockLookup = Symbol -> InstructionSet.BlockOffset voption

type ParsedInstruction =
    RegisterLookup -> IInstructionResolver -> InstructionErrorsBuilder -> CodeBlockLookup -> InstructionSet.Instruction voption

[<RequireQualifiedAccess>]
type ParsedExceptionHandler = | Finally of handler: Symbol | Catch of Symbol * handler: Symbol | None

type ParsedBlock =
    { ExceptionHandler: ParsedExceptionHandler
      Symbol: Symbol
      Instructions: struct(ParsedRegister list * ParsedInstruction) list }

type ParsedCode = { Locals: Symbol list; Arguments: Symbol voption list; Blocks: ParsedBlock list }


let code: Parser<ParsedCode, _> =
    let lsymbol = nsymbol '$'

    let localCodeRegister = choice [
        lsymbol |>> (fun name -> { IsTemporary = false; Name = name }) <?> "local or argument register"
        nsymbol '%' |>> (fun name -> { IsTemporary = true; Name = name }) <?> "temporary register"
    ]

    let codeRegisterList = commaSepInsideParens localCodeRegister

    let manyCodeRegisters eq = choice [
        codeRegisterList .>> eq
        localCodeRegister .>> eq |>> List.singleton
        preturn List.empty
    ]

    let inline addErrorTo (errors: InstructionErrorsBuilder) e =
        errors.Add e
        ValueNone

    let lookupRegisterName (lookup: RegisterLookup) errors ({ ParsedRegister.Name = id } as reg): RegisterIndex voption =
        match lookup reg with
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

    let lookupModuleData (lookup: IInstructionResolver) errors id =
        match lookup.FindData id with
        | ValueSome _ as i -> i
        | ValueNone -> addErrorTo errors (InvalidInstructionError.UndefinedData id)

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

    let lookupCodeBlock (lookup: CodeBlockLookup) errors label =
        let i = lookup label
        if i.IsNone then addErrorTo errors (InvalidInstructionError.UndefinedBlock label) |> ignore
        i

    let codeInstructionName = many1Chars (choice [ asciiLetter; digit; pchar '.' ])

    let instruction: Parser<ParsedInstruction, _> =
        let unknown pos name: Parser<ParsedInstruction, _> =
            fun _ _ errors _ ->
                addErrorTo errors (InvalidInstructionError.UnknownInstruction(pos, name))
            |> preturn

        let instructions = System.Collections.Generic.Dictionary()

        let addInstructionParser name parser = instructions.Add(name, parser)
        let noOperandInstruction name instr = addInstructionParser name (preturn (fun _ _ _ _ -> ValueSome instr))
        noOperandInstruction "nop" InstructionSet.Nop
        noOperandInstruction "obj.null" InstructionSet.Obj_null

        let commonArithmeticFlags = optattributes [ "throw.ovf", InstructionSet.ArithmeticFlags.ThrowOnOverflow ]
        let divisionArithmeticFlags = optattributes [ "throw.div0", InstructionSet.ArithmeticFlags.ThrowOnDivideByZero ]

        let withRegisterCount name count instr =
            parray count localCodeRegister |>> fun registers rlookup _ errors _ -> voptional {
                let! registers' = lookupRegisterArray rlookup registers errors
                return instr registers'
            }
            |> addInstructionParser name

        let withTwoRegisters name instr =
            withRegisterCount name 2 (fun registers -> instr(registers.[0], registers.[1]))

        withTwoRegisters "obj.arr.get" InstructionSet.Obj_arr_get

        let withThreeRegisters name instr =
            withRegisterCount name 3 (fun registers -> instr(registers.[0], registers.[1], registers.[2]))

        withThreeRegisters "obj.arr.set" InstructionSet.Obj_arr_set

        let arithmeticUnaryInstruction name flags instr =
            pipe3 flags tprimitive localCodeRegister <| fun aflags vtype reg rlookup _ errors _ ->
                voptional {
                    let! reg' = lookupRegisterName rlookup errors reg
                    return instr(aflags, vtype, reg')
                }
            |> addInstructionParser name

        arithmeticUnaryInstruction "incr" commonArithmeticFlags InstructionSet.Incr
        arithmeticUnaryInstruction "decr" commonArithmeticFlags InstructionSet.Decr
        arithmeticUnaryInstruction "obj.arr.len" commonArithmeticFlags InstructionSet.Obj_arr_len

        let arithmeticBinaryInstruction name flags instr =
            pipe4 flags tprimitive localCodeRegister localCodeRegister <| fun aflags vtype xreg yreg rlookup _ errors _ ->
                voptional {
                    let! xreg' = lookupRegisterName rlookup errors xreg
                    let! yreg' = lookupRegisterName rlookup errors yreg
                    return instr(aflags, vtype, xreg', yreg')
                }
            |> addInstructionParser name

        arithmeticBinaryInstruction "add" commonArithmeticFlags InstructionSet.Add
        arithmeticBinaryInstruction "sub" commonArithmeticFlags InstructionSet.Sub
        arithmeticBinaryInstruction "mul" commonArithmeticFlags InstructionSet.Mul
        arithmeticBinaryInstruction "div" divisionArithmeticFlags InstructionSet.Div
        arithmeticBinaryInstruction "rem" divisionArithmeticFlags InstructionSet.Rem

        let bitwiseUnaryInstruction name instr =
            pipe2 tprimitive localCodeRegister <| fun vtype reg rlookup _ errors _ -> voptional {
                let! reg' = lookupRegisterName rlookup errors reg
                return instr(vtype, reg')
            }
            |> addInstructionParser name

        bitwiseUnaryInstruction "not" InstructionSet.Not

        let bitwiseBinaryInstruction name instr =
            pipe3 tprimitive localCodeRegister localCodeRegister <| fun vtype xreg yreg rlookup _ errors _ -> voptional {
                let! xreg' = lookupRegisterName rlookup errors xreg
                let! yreg' = lookupRegisterName rlookup errors yreg
                return instr(vtype, xreg', yreg')
            }
            |> addInstructionParser name

        bitwiseBinaryInstruction "and" InstructionSet.And
        bitwiseBinaryInstruction "or" InstructionSet.Or
        bitwiseBinaryInstruction "xor" InstructionSet.Xor
        bitwiseBinaryInstruction "rotl" InstructionSet.Rotl
        bitwiseBinaryInstruction "rotr" InstructionSet.Rotr

        let constantBooleanInstruction name instr =
            tprimitive |>> (fun vtype _ _ _ _ -> ValueSome(instr vtype)) |> addInstructionParser name

        constantBooleanInstruction "const.zero" InstructionSet.Const_zero
        constantBooleanInstruction "const.true" InstructionSet.Const_true
        constantBooleanInstruction "const.false" InstructionSet.Const_false

        let constantNumberInstruction name (number: Parser<NumberLiteral, _>) instr num =
            pipe3 getPosition tprimitive number <| fun pos vtype value rlookup _ errors _ -> voptional {
                let! value' =
                    match num value with
                    | Result.Ok i -> ValueSome i
                    | Result.Error err -> addErrorTo errors (err pos)

                return instr(vtype, value')
            }
            |> addInstructionParser name

        constantNumberInstruction "const.s" integerlit InstructionSet.Const_s <| fun value ->
            match Int64.TryParse value.String with
            | true, i when i >= int64 Int32.MinValue && i <= int64 UInt32.MaxValue -> Result.Ok(int32 i)
            | _ -> Result.Error(fun pos -> InvalidInstructionError.InvalidIntegerLiteral(pos, sizeof<int32>, value.String))

        constantNumberInstruction "const.u" integerlit InstructionSet.Const_u <| fun value ->
            match Int64.TryParse value.String with
            | true, i when i >= int64 UInt32.MinValue && i <= int64 UInt32.MaxValue -> Result.Ok(uint32 i)
            | _ -> Result.Error(fun pos -> InvalidInstructionError.InvalidIntegerLiteral(pos, sizeof<int32>, value.String))

        //"const.f32"
        //"const.f64"

        let branchComparisonInstruction name instr =
            pipe4
                localCodeRegister
                localCodeRegister
                (keyword "then" >>. lsymbol)
                (keyword "else" >>. lsymbol)
                (fun xreg yreg btrue bfalse rlookup _ errors blocks -> voptional {
                    let! xreg' = lookupRegisterName rlookup errors xreg
                    let! yreg' = lookupRegisterName rlookup errors yreg
                    let! btrue' = lookupCodeBlock blocks errors btrue
                    let! bfalse' = lookupCodeBlock blocks errors bfalse
                    return instr(xreg', yreg', btrue', bfalse')
                })
            |> addInstructionParser name

        branchComparisonInstruction "br.eq" InstructionSet.Br_eq
        branchComparisonInstruction "br.ne" InstructionSet.Br_ne
        branchComparisonInstruction "br.lt" InstructionSet.Br_lt
        branchComparisonInstruction "br.gt" InstructionSet.Br_gt
        branchComparisonInstruction "br.le" InstructionSet.Br_le
        branchComparisonInstruction "br.ge" InstructionSet.Br_ge

        let callInstructionFlags extra = optattributes [
            "tail.prohibited", InstructionSet.CallFlags.NoTailCallOptimization
            "tail.required", InstructionSet.CallFlags.RequiresTailCallOptimization
            yield! extra
        ]

        pipe3 (callInstructionFlags List.empty) symbol codeRegisterList <| fun flags method args rlookup resolver errors _ ->
            voptional {
                let! method' = lookupMethodName resolver errors method
                let! arguments = lookupRegisterList rlookup errors args
                return InstructionSet.Call(flags, method', arguments)
            }
        |> addInstructionParser "call"

        pipe4
            (callInstructionFlags [ "throw.nullthis", InstructionSet.CallFlags.ThrowOnNullThis ])
            symbol
            localCodeRegister
            codeRegisterList
            (fun flags method treg args rlookup resolver errors _ -> voptional {
                let! method' = lookupMethodName resolver errors method
                let! treg' = lookupRegisterName rlookup errors treg
                let! arguments = lookupRegisterList rlookup errors args
                return InstructionSet.Call_virt(flags, method', treg', arguments)
            })
        |> addInstructionParser "call.virt"

        pipe2 symbol localCodeRegister <| fun field oreg rlookup resolver errors _ -> voptional {
            let! field' = lookupFieldName resolver errors field
            let! oreg' = lookupRegisterName rlookup errors oreg
            return InstructionSet.Obj_fd_ld(field', oreg')
        }
        |> addInstructionParser "obj.fd.ld"

        pipe3 symbol localCodeRegister localCodeRegister <| fun field oreg sreg rlookup resolver errors _ -> voptional {
            let! field' = lookupFieldName resolver errors field
            let! oreg' = lookupRegisterName rlookup errors oreg
            let! sreg' = lookupRegisterName rlookup errors sreg
            return InstructionSet.Obj_fd_st(field', oreg', sreg')
        }
        |> addInstructionParser "obj.fd.st"

        pipe3
            localCodeRegister
            (keyword "then" >>. lsymbol)
            (keyword "else" >>. lsymbol)
            (fun creg btrue bfalse rlookup _ errors blocks -> voptional {
                let! creg' = lookupRegisterName rlookup errors creg
                let! btrue' = lookupCodeBlock blocks errors btrue
                let! bfalse' = lookupCodeBlock blocks errors bfalse
                return InstructionSet.Br_true(creg', btrue', bfalse')
            })
        |> addInstructionParser "br.true"

        lsymbol |>> fun i _ _ errors labels -> voptional {
            let! i' = lookupCodeBlock labels errors i
            return InstructionSet.Br i'
        }
        |> addInstructionParser "br"

        let phiSelectionPair = localCodeRegister .>> keyword "when" .>>. lsymbol

        sepBy1 phiSelectionPair (keyword "or") |>> fun selections rlookup _ errors blocks -> voptional {
            let rec inner selections (values: ImmutableArray<_>.Builder) success =
                match selections with
                | [] when success -> ValueSome(values.ToImmutable())
                | [] -> ValueNone
                | (register, block) :: remaining ->
                    let result = voptional {
                        let! register' = lookupRegisterName rlookup errors register
                        let! block' = lookupCodeBlock blocks errors block
                        values.Add(struct(register', block'))
                        return ValueSome()
                    }
                    inner remaining values (success && result.IsSome)

            let! selections' = inner selections (ImmutableArray.CreateBuilder()) true
            return InstructionSet.Phi selections'
        }
        |> addInstructionParser "phi"

        manyCodeRegisters (preturn ()) |>> fun registers rlookup _ errors _ -> voptional {
            let! registers' = lookupRegisterList rlookup errors registers
            return InstructionSet.Ret registers'
        }
        |> addInstructionParser "ret"

        pipe2 symbol codeRegisterList <| fun ctor args rlookup resolver errors _ -> voptional {
            let! constructor' = lookupMethodName resolver errors ctor
            let! arguments = lookupRegisterList rlookup errors args
            return InstructionSet.Obj_new(constructor', arguments.ToImmutableArray())
        }
        |> addInstructionParser "obj.new"

        pipe2 symbol localCodeRegister <| fun etype lreg rlookup resolver errors _ -> voptional {
            let! etype' = lookupTypeSignature resolver errors etype
            let! lreg' = lookupRegisterName rlookup errors lreg
            return InstructionSet.Obj_arr_new(etype', lreg')
        }
        |> addInstructionParser "obj.arr.new"

        localCodeRegister |>> fun exreg rlookup _ errors _ -> voptional {
            let! exreg' = lookupRegisterName rlookup errors exreg
            return InstructionSet.Obj_throw exreg'
        }
        |> addInstructionParser "obj.throw"

        pipe2 symbol symbol <| fun etype data _ resolver errors _ -> voptional {
            let! etype' = lookupTypeSignature resolver errors etype
            let! data' = lookupModuleData resolver errors data
            return InstructionSet.Obj_arr_const(etype', data')
        }
        |> addInstructionParser "obj.arr.const"

        getPosition .>>. codeInstructionName .>> whitespace >>= fun (pos, name) ->
            match instructions.TryGetValue name with
            | true, instr -> instr
            | false, _ -> unknown pos name .>> skipRestOfLine true

    let manyLocalRegisters word register = choice [
        period >>. keyword word |> attempt >>. commaSepInsideParens register
        preturn List.empty
    ]

    let cblock =
        let instr =
            pipe2
                (manyCodeRegisters (whitespace >>. skipChar '=' .>> whitespace))
                instruction
                (fun rregs instr -> struct(rregs, instr))
            |> line
            |> many
            |> block

        let exhandler = choice [
            keyword "catches" >>. lsymbol .>> keyword "with" .>>. lsymbol |>> ParsedExceptionHandler.Catch
            keyword "finally" >>. lsymbol |>> ParsedExceptionHandler.Finally // TODO: Pick better keyword for finally clause.
            preturn ParsedExceptionHandler.None
        ]

        pipe3 (period >>. keyword "block" >>. nsymbol '$') exhandler instr <| fun name eh instrs ->
            { ParsedBlock.ExceptionHandler = eh
              Symbol = name
              Instructions = instrs }

    pipe3
        (manyLocalRegisters "arguments" (optsymbol '$'))
        (manyLocalRegisters "locals" (nsymbol '$'))
        (many1 cblock)
        (fun arguments locals blocks -> { ParsedCode.Locals = locals; Arguments = arguments; Blocks = blocks })
    |> block

type ParsedNamespace = { NamespaceName: Symbol list }

let visibility: Parser<VisibilityFlags, _> =
    attributes [
        "public", VisibilityFlags.Public
        "private", VisibilityFlags.Private
    ]

[<RequireQualifiedAccess>]
type TypeDefAttr = | Visibility of Position * VisibilityFlags | Flag of Position * TypeDefinitionFlags

let tflags = getPosition .>>. attributes [
    "nonfinal", TypeDefinitionFlags.NotFinal
    "abstract", TypeDefinitionFlags.Abstract
    "objectonly", TypeDefinitionFlags.ReferenceOnly
    "stackonly", TypeDefinitionFlags.StackOnly
]

let tdefattr =
    choice [
        getPosition .>>. visibility |>> TypeDefAttr.Visibility
        tflags |>> TypeDefAttr.Flag
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
    "virtual", MethodFlags.Virtual
]

let mdefattr =
    choice [
        getPosition .>>. visibility |>> MethodDefAttr.Visibility
        mflags |>> MethodDefAttr.Flag
    ]
    .>> whitespace
    |> many

[<RequireQualifiedAccess>]
type ParsedMethodBody =
    | Defined of ((Symbol -> CodeIndex voption) -> Result<MethodBody, Name>)
    | External of ((Symbol -> IdentifierIndex voption) -> Result<MethodBody, Name>)

[<RequireQualifiedAccess>]
type MethodDefDecl =
    | Signature of Symbol
    | Name of Symbol
    | Body of Position * ParsedMethodBody

let mdefdecl =
    period >>. choice [
        namedecl' |>> MethodDefDecl.Name
        keyword "signature" >>. symbol |>> MethodDefDecl.Signature
        keyword "body" >>. choice [
            keyword "defined" >>. symbol |>> fun ((pos, coden) as body) ->
                let defined lookup =
                    match lookup body with
                    | ValueSome codei -> Result.Ok(MethodBody.Defined codei)
                    | ValueNone -> Result.Error coden

                MethodDefDecl.Body(pos, ParsedMethodBody.Defined defined)

            getPosition .>> keyword "abstract" |>> fun pos ->
                MethodDefDecl.Body(pos, ParsedMethodBody.Defined(fun _ -> Result.Ok MethodBody.Abstract))

            pipe3
                (getPosition .>> keyword "external")
                (symbol .>> whitespace .>> keyword "from" .>> whitespace)
                symbol
                (fun pos ((_, funcn) as efunction) ((_, libn) as library) ->
                    let external lookup =
                        match lookup library, lookup efunction with
                        | ValueSome libi, ValueSome funci -> Result.Ok(MethodBody.External(libi, funci))
                        | ValueNone, _ -> Result.Error libn
                        | _, ValueNone -> Result.Error funcn

                    MethodDefDecl.Body(pos, ParsedMethodBody.External external))
        ]
    ]
    |> line
    |> many

[<RequireQualifiedAccess>]
type TypeDefDecl =
    | Name of Symbol
    | Namespace of Symbol
    | Extends of Symbol
    | Field of Symbol voption * FieldDefAttr list * FieldDefDecl list
    | Method of Symbol voption * MethodDefAttr list * MethodDefDecl list
    | MethodOverride of implementation: Symbol * declaration: Symbol

let tdefdecl =
    period >>. choice [
        keyword "namespace" >>. symbol .>> whitespace |>> TypeDefDecl.Namespace
        namedecl' |>> TypeDefDecl.Name
        keyword "extends" >>. symbol .>> whitespace |>> TypeDefDecl.Extends
        keyword "field" >>. tuple3 (symbol |>> ValueSome) fdefattr (block fdefdecl) |>> TypeDefDecl.Field
        keyword "method" >>. tuple3 (symbol |>> ValueSome) mdefattr (block mdefdecl) |>> TypeDefDecl.Method
        keyword "override" >>. symbol .>> keyword "for" .>>. symbol |>> TypeDefDecl.MethodOverride
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
    | Data of Symbol * seq<byte>

let tdata = choice [
    let str name (encoding: System.Text.Encoding) = keyword name >>. stringlit |>> (encoding.GetBytes >> Seq.ofArray)

    str "utf8" System.Text.Encoding.UTF8
    str "utf16le" System.Text.Encoding.Unicode
    str "utf16be" System.Text.Encoding.BigEndianUnicode
    str "utf32le" System.Text.Encoding.UTF32
    //str "utf32be"

    let hexbyte =
        let hexval (d1: char) = uint8 d1 - (if d1 <= '9' then uint8 '0' else uint8 'A' - 0xAuy)
        pipe2 hex hex (fun d1 d2 -> (16uy * hexval d1) + hexval d2)
        <?> "hexadecimal byte"

    keyword "bytes" >>. block (many hexbyte) |>> List.toSeq
]

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
        keyword "data" >>. symbol .>>. tdata |>> ParsedDeclaration.Data
    ]
    // TODO: If declaration could not be parsed, store error and parse the next one

    many (whitespace >>. declaration .>> whitespace) .>> eof
