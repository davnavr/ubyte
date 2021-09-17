module UByte.Assembler.Assembler

open System
open System.Collections.Generic
open System.Collections.Immutable

open UByte.Format.Model

type Position = FParsec.Position

let atomErrorMsg =
    let format = sprintf "\"%O\" at %O"
    fun { Parser.Atom = atom; Parser.Position = pos } -> format atom pos

type AssemblerError =
    | ExpectedKeyword of expected: string * actual: Parser.PositionedAtom option
    | DuplicateFormatVersion of duplicate: Parser.PositionedAtom
    | InvalidVersionNumber of number: Parser.PositionedAtom
    | MissingModuleName of name: Parser.PositionedAtom voption
    | DuplicateModuleName of duplicate: Parser.PositionedAtom
    | DuplicateModuleVersion of duplicate: Parser.PositionedAtom
    | SymbolNotDefined of symbol: Name * Parser.PositionedAtom
    | EmptyIdentifierEntry of identifier: Parser.PositionedAtom
    | DuplicateIdentifierSymbol of symbol: Name * identifier: Parser.PositionedAtom
    | InvalidSignatureKind of signature: Parser.PositionedAtom
    | MissingTypeSignature of Parser.PositionedAtom
    | InvalidTypeSignature of ``type``: Parser.PositionedAtom
    | InvalidMethodSignature of method : Parser.PositionedAtom
    | DuplicateSignatureSymbol of symbol: Name * code: Parser.PositionedAtom
    | UnknownInstruction of name: string * instruction: Parser.PositionedAtom
    | InvalidInstruction of instruction: Parser.PositionedAtom
    | DuplicateCodeSymbol of symbol: Name * code: Parser.PositionedAtom
    | MissingInstructionArguments of instruction: Parser.PositionedAtom * count: int32
    | DuplicateRegisterSymbol of symbol: Name * register: Parser.PositionedAtom
    | DuplicateTypeSymbol of symbol: Name * Parser.PositionedAtom
    | MissingTypeKind of Parser.PositionedAtom
    | InvalidVisibility of visibility: Parser.PositionedAtom
    | DuplicateMethodSymbol of symbol: Name * method: Parser.PositionedAtom
    | MissingMethodSignature of method: Parser.PositionedAtom
    | MissingMethodKind of method: Parser.PositionedAtom
    | DuplicateEntryPoint of Parser.PositionedAtom
    | InvalidEntryPoint of Parser.PositionedAtom
    | UnexpectedAtom of Parser.PositionedAtom

    override this.ToString() =
        match this with
        | ExpectedKeyword(expected, actual) ->
            Option.map atomErrorMsg actual
            |> Option.defaultValue "end of file"
            |> sprintf "Expected keyword \"%s\" but got %s" expected
        | DuplicateFormatVersion { Parser.Position = pos } ->
            sprintf "Duplicate format version at %O" pos
        | InvalidVersionNumber atom ->
            atomErrorMsg atom
            + "is not a valid version number, versions are specified as a list of positive integers"
        | MissingModuleName atom ->
            (ValueOption.map atomErrorMsg atom |> ValueOption.defaultValue "The module") +
            " is missing the module name, module names are of the form (name \"my.module.name\")"
        | DuplicateModuleName { Parser.Position = pos } ->
            sprintf "Duplicate module name at %O" pos
        | DuplicateModuleVersion { Parser.Position = pos } ->
            sprintf "Duplicate module version at %O" pos
        | SymbolNotDefined(symbol, atom) ->
            sprintf "The symbol $%O in %s is not defined" symbol (atomErrorMsg atom)
        | EmptyIdentifierEntry atom ->
            atomErrorMsg atom +
            "is not a valid identifier, identifiers should be of the form (identifier $my_name_symbol \"MyName\") or " +
            "(identifier \"MyName\")"
        | DuplicateIdentifierSymbol(symbol, { Parser.Position = pos }) ->
            sprintf "Duplicate identifier at %O, an identifier corresponding to the symbol $%O already exists" pos symbol
        | InvalidSignatureKind atom ->
            atomErrorMsg atom + " is an invalid signature, a valid signature should be of the form (signature " +
            "$my_signature_symbol (type ...)) where ... indicates a type signature or (signature $my_signature_symbol (method " +
            "...)) where ... indicates a method signature"
        | MissingTypeSignature atom ->
            atomErrorMsg atom
            + "is not a valid type, a valid type should be of the form (type ...) where ... indicates a type signature"
        | InvalidTypeSignature atom ->
            atomErrorMsg atom + " is an invalid type signature, a valid type should be a primitive (s32, bool, char16, etc.)"
        | InvalidMethodSignature atom ->
            atomErrorMsg atom + " is an invalid method signature, method signatures are of the form (method (returns "
            + "$my_return_type_1 $my_return_type_2) (parameters $my_param_type_1 $my_param_type_2)"
        | DuplicateSignatureSymbol(symbol, { Parser.Position = pos }) ->
            sprintf "Duplicate symbol at %O, a type or method signature corresponding to the symbol $%O already exists" pos symbol
        | UnknownInstruction(name, instr) ->
            sprintf "%s is an unknown instruction, \"%s\" does not refer to any valid instruction" (atomErrorMsg instr) name
        | InvalidInstruction instr ->
            atomErrorMsg instr + " is not a valid instruction"
        | DuplicateCodeSymbol(symbol, { Parser.Position = pos }) ->
            sprintf "Duplicate symbol at %O, a code block corresponding to the symbol $%O already exists" pos symbol
        | MissingInstructionArguments(instr, count) ->
            sprintf "The instruction %s is missing %i arguments" (atomErrorMsg instr) count
        | DuplicateRegisterSymbol(symbol, { Parser.Position = pos }) ->
            sprintf "Duplicate symbol at %O, a register corresponding to the symbol $%O already exists in this code" pos symbol
        | DuplicateTypeSymbol(symbol, { Parser.Position = pos }) ->
            sprintf "Duplicate symbol at %O, a type definition corresponding to the symbol $%O already exists" pos symbol
        | MissingTypeKind atom ->
            atomErrorMsg atom + " is missing a type kind, specifiy the kind of the type with (class), (struct), or (interface)"
        | InvalidVisibility atom ->
            atomErrorMsg atom + " is not a valid visibility value, a valid visibility value is of the form (visibility public)" +
            ", (visibility private), or (visibility unspecified)"
        | DuplicateMethodSymbol(symbol, { Parser.Position = pos }) ->
            sprintf "Duplicate symbol at %O, a method corresponding to the symbol $%O already exists" pos symbol
        | MissingMethodSignature method ->
            atomErrorMsg method + " is missing the method signature (signature $my_method_signature)"
        | MissingMethodKind method ->
            atomErrorMsg method + " is missing the method kind, which is of the form (defined $my_method_body) for methods " +
            "with an implementation"
        | DuplicateEntryPoint { Parser.Position = pos } ->
            sprintf "Duplicate entry point at %O" pos
        | InvalidEntryPoint atom ->
            atomErrorMsg atom + " is not a valid entry point, specify the entry point with (entrypoint $my_main_method)"
        | UnexpectedAtom atom -> "Unexpected " + atomErrorMsg atom

[<Sealed>]
type SymbolDictionary<'IndexKind, 'T when 'IndexKind :> IndexKinds.Kind> () =
    let items = ImmutableArray.CreateBuilder<'T>()
    let symbols = Dictionary<Name, int32>()

    member _.Count = items.Count

    member _.AddAnonymous item: Index<'IndexKind> =
        let i = items.Count
        items.Add item
        Index(uint32 i)

    member this.AddNamed(symbol, item) =
        match symbols.TryGetValue symbol with
        | false, _ ->
            let (Index i) = this.AddAnonymous item
            symbols.Add(symbol, Checked.int32 i)
            None
        | true, _ -> Some(fun a -> symbol, a) 

    member this.Add(symbol, item): (Parser.PositionedAtom -> _ * _) option =
        match symbol with
        | ValueSome symbol -> this.AddNamed(symbol, item)
        | ValueNone -> this.AddAnonymous item |> ignore; None

    member _.FromSymbol symbol: Index<'IndexKind> voption =
        match symbols.TryGetValue symbol with
        | true, item -> ValueSome(Index(Checked.uint32 item))
        | false, _ -> ValueNone

    member _.ToVector(): vector<'T> = items.ToImmutable()

    member _.GetEnumerator() = items.GetEnumerator()

[<Sealed>]
type TypeDefinitionLookup () =
    let defined = SymbolDictionary<IndexKinds.TypeDefinition, TypeDefinition>()
    //let imported = SymbolDictionary<IndexKinds.TypeDefinition, TypeDefinitionImport>()

    member _.AddDefinition(symbol, item) = defined.Add(symbol, item)

[<Sealed>]
type MethodLookup () =
    let defined = SymbolDictionary<IndexKinds.Method, Method>()
    //let imported

    member _.AddDefinition(symbol, item) = defined.Add(symbol, item)
    member _.FindDefinition symbol = defined.FromSymbol symbol
    member _.ToVector() = defined.ToVector()

[<Struct>]
type NamespaceLookupEnumerator =
    val mutable private enumerator: Dictionary<vector<IdentifierIndex>, ImmutableArray<TypeDefinition>.Builder * ImmutableArray<TypeAlias>.Builder>.Enumerator

    new (lookup: Dictionary<_, _>) = { enumerator = lookup.GetEnumerator() }

    member this.Current =
        let (KeyValue(ns, (tdefs, taliases))) = this.enumerator.Current
        { Namespace.NamespaceName = ns
          TypeDefinitions = tdefs.ToImmutable()
          TypeAliases = taliases.ToImmutable() }

    member this.MoveNext() = this.enumerator.MoveNext()
    
    interface IEnumerator<Namespace> with
        member this.Current = this.Current
        member this.Current = this.Current :> obj
        member this.MoveNext() = this.MoveNext()
        member this.Reset() = (this.enumerator :> IEnumerator<_>).Reset()
        member this.Dispose() = this.enumerator.Dispose()

[<Sealed>]
type NamespaceLookup () =
    let namespaces = Dictionary<ImmutableArray<_>, ImmutableArray<TypeDefinition>.Builder * ImmutableArray<TypeAlias>.Builder> LanguagePrimitives.FastGenericEqualityComparer

    member _.Count = namespaces.Count

    member private _.GetNamespace ns =
        match namespaces.TryGetValue ns with
        | true, existing -> existing
        | false, _ ->
            let lists = ImmutableArray.CreateBuilder(), ImmutableArray.CreateBuilder()
            namespaces.Add(ns, lists)
            lists

    member this.AddDefinition(ns, tdef) = (fst(this.GetNamespace ns)).Add tdef

    //member _.AddAlias

    member _.GetEnumerator() = new NamespaceLookupEnumerator(namespaces)

    interface IReadOnlyCollection<Namespace> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator

[<NoComparison; NoEquality>]
type State =
    { ModuleFormatVersion: VersionNumbers voption ref
      mutable ModuleHeaderName: Name voption
      ModuleHeaderVersion: VersionNumbers voption ref
      ModuleIdentifiers: SymbolDictionary<IndexKinds.Identifier, string>
      ModuleTypeSignatures: SymbolDictionary<IndexKinds.TypeSignature, AnyType>
      ModuleMethodSignatures: SymbolDictionary<IndexKinds.MethodSignature, MethodSignature>
      ModuleCode: SymbolDictionary<IndexKinds.Code, Code>
      ModuleNamespaces: NamespaceLookup
      ModuleTypes: TypeDefinitionLookup
      ModuleMethods: MethodLookup
      ModuleEntryPoint: MethodIndex voption ref }

let inline (|Atom|) { Parser.Atom = atom } = atom

type ExpressionParserResult = System.ValueTuple<Parser.PositionedAtom list, AssemblerError list>

let extraneous atoms errors =
    Seq.fold (fun errors extra -> UnexpectedAtom extra :: errors) errors atoms

let keyword name contents atoms (errors: AssemblerError list) (state: State): ExpressionParserResult option =
    let contents' watom nested atoms': struct(_ * _) option =
        match contents watom nested errors state with
        | struct([], errors') -> Some(atoms', errors')
        | extra, errors' -> Some(atoms', extraneous extra errors')

    match atoms with
    | Atom(Parser.Keyword word) as watom :: atoms' when word = name ->
        contents' watom List.empty atoms'
    | Atom(Parser.NestedAtom(Atom(Parser.Keyword word) as watom :: nested)) :: atoms' when word = name ->
        contents' watom nested atoms'
    | _ -> None

let keywordWithSymbol name contents =
    keyword
        name
        (fun katom atoms errors state ->
            match atoms with
            | Atom(Parser.Identifier symbol) :: remaining ->
                contents (ValueSome (Name.ofStr symbol)) katom remaining errors state
            | _ ->
                contents ValueNone katom atoms errors state)

let emptyFormatVersion _ state =
    state.ModuleFormatVersion.contents <- ValueSome VersionNumbers.empty
    None

let emptyHeaderVersion _ state =
    state.ModuleHeaderVersion.contents <- ValueSome VersionNumbers.empty
    None

let versionNumberVector field atoms errors (_: State) =
    let rec inner (numbers: ImmutableArray<_>.Builder) field atoms errors =
        match atoms with
        | [] ->
            field.contents <- ValueSome(VersionNumbers(numbers.ToImmutable()))
            struct([], errors)
        | Atom(Parser.IntegerLiteral n) :: remaining when n >= 0L ->
            numbers.Add(uint32 n)
            inner numbers field remaining errors
        | bad :: remaining ->
            inner numbers field remaining (InvalidVersionNumber bad :: errors)
    inner (ImmutableArray.CreateBuilder()) field atoms errors

let (|ParsedIdentifier|) (Atom atom) =
    match atom with
    | Parser.StringLiteral name -> Name.tryOfStr name
    | _ -> ValueNone

let (|ParsedTypeSignature|) atoms =
    let inline primitive remaining t: struct(_ * _) voption = ValueSome(remaining, AnyType.Primitive t)
    match atoms with
    | Atom(Parser.Keyword "bool") :: atoms' -> primitive atoms' PrimitiveType.Bool
    | Atom(Parser.Keyword "s8") :: atoms' -> primitive atoms' PrimitiveType.S8
    | Atom(Parser.Keyword "u8") :: atoms' -> primitive atoms' PrimitiveType.U8
    | Atom(Parser.Keyword "s16") :: atoms' -> primitive atoms' PrimitiveType.S16
    | Atom(Parser.Keyword "u16") :: atoms' -> primitive atoms' PrimitiveType.U16
    | Atom(Parser.Keyword "char16") :: atoms' -> primitive atoms' PrimitiveType.Char16
    | Atom(Parser.Keyword "s32") :: atoms' -> primitive atoms' PrimitiveType.S32
    | Atom(Parser.Keyword "u32") :: atoms' -> primitive atoms' PrimitiveType.U32
    | Atom(Parser.Keyword "char32") :: atoms' -> primitive atoms' PrimitiveType.Char32
    | Atom(Parser.Keyword "s64") :: atoms' -> primitive atoms' PrimitiveType.S64
    | Atom(Parser.Keyword "u64") :: atoms' -> primitive atoms' PrimitiveType.U64
    | Atom(Parser.Keyword "f32") :: atoms' -> primitive atoms' PrimitiveType.F32
    | Atom(Parser.Keyword "f64") :: atoms' -> primitive atoms' PrimitiveType.F64
    | Atom(Parser.Keyword "unit") :: atoms' -> primitive atoms' PrimitiveType.Unit
    | _ -> ValueNone

let (|ParsedMethodSignature|) matom errors state atoms =
    let (|ParsedMethodTypes|) name errors atoms: struct(_ * _) voption =
        match atoms with
        | Atom(Parser.NestedAtom(Atom(Parser.Keyword keyword) :: types)) as matom :: atoms' when keyword = name ->
            let rec inner (builder: ImmutableArray<_>.Builder) types errors: struct(_ * _) voption =
                match types with
                | Atom(Parser.Identifier tname) as tatom :: types' ->
                    let tname' = Name.ofStr tname
                    match state.ModuleTypeSignatures.FromSymbol tname' with
                    | ValueSome t ->
                        builder.Add t
                        inner builder types' errors
                    | ValueNone ->
                        ValueSome([], Error(SymbolNotDefined(tname', tatom) :: errors))
                | [] ->
                    ValueSome([], Ok(builder.ToImmutable()))
                | _ ->
                    ValueSome(atoms', Error(InvalidMethodSignature matom :: errors))
            inner (ImmutableArray.CreateBuilder()) types errors
        | [] as atoms'
        | Atom(Parser.Keyword _) :: atoms'
        | Atom(Parser.NestedAtom [ Atom(Parser.Keyword _) ]) :: atoms' ->
            // Keyword is not checked as list of return types might be omitted.
            ValueSome(atoms', Ok ImmutableArray.Empty)
        | _ -> ValueNone

    let invalidMethodSignature = Error(InvalidMethodSignature matom :: errors)

    match atoms with
    | ParsedMethodTypes "returns" errors (ValueSome(atoms', Ok rtypes)) ->
        match atoms' with
        | ParsedMethodTypes "parameters" errors (ValueSome([], Ok ptypes)) ->
            Ok { ReturnTypes = rtypes; ParameterTypes = ptypes }
        | ParsedMethodTypes "parameters" errors (ValueSome(_, Error errors')) ->
            Error errors'
        | _ -> invalidMethodSignature
    | _ -> invalidMethodSignature

let rec instructions body errors state =
    let registers = SymbolDictionary<IndexKinds.Register, _>()
    let instrs = ImmutableArray.CreateBuilder<InstructionSet.Instruction>()

    let operation name count body args: struct(_ * _) voption =
        match body with
        | Atom(Parser.Keyword op) :: body' when op = name ->
            ValueSome(body', [ failwith "TODO: Error for missing argument" ])
        | Atom(Parser.NestedAtom(Atom(Parser.Keyword op) :: arguments)) as opatom :: body' when op = name ->
            let arguments' = List.toArray arguments
            let extra = count - arguments'.Length
            if arguments'.Length >= count then
                match args opatom arguments' with
                | Ok instr ->
                    instrs.Add instr

                    let errors' =
                        if arguments'.Length = count
                        then []
                        else extraneous (List.skip extra arguments) []

                    ValueSome(body', errors')
                | Error err ->
                    ValueSome(body', [ err ])
            else
                ValueSome(body', [ MissingInstructionArguments(opatom, extra) ])
        | _ -> ValueNone

    let rec inner body errors =
        let (|Op1|) name op body =
            operation name 1 body <| fun iatom args ->
                match args.[0] with
                | Atom(Parser.Identifier rname) ->
                    let rname' = Name.ofStr rname
                    match registers.FromSymbol rname' with
                    | ValueSome r' -> Ok(op r')
                    | ValueNone -> Error(SymbolNotDefined(rname', iatom))
                | bad ->
                    Error(UnexpectedAtom bad)

        let (|Op3|) name op body =
            operation name 3 body <| fun iatom args ->
                match args with
                | [| Atom(Parser.Identifier xr); Atom(Parser.Identifier yr); Atom(Parser.Identifier rr) |] ->
                    let xr' = Name.ofStr xr
                    let yr' = Name.ofStr yr
                    let rr' = Name.ofStr rr
                    match registers.FromSymbol xr', registers.FromSymbol yr', registers.FromSymbol rr' with
                    | ValueSome x, ValueSome y, ValueSome r -> Ok(op(x, y, r))
                    | ValueNone, _, _ -> Error(SymbolNotDefined(xr', iatom))
                    | _, ValueNone, _ -> Error(SymbolNotDefined(yr', iatom))
                    | _, _, ValueNone -> Error(SymbolNotDefined(rr', iatom))
                | [| badx; Atom(Parser.Identifier _); Atom(Parser.Identifier _) |] ->
                    Error(UnexpectedAtom badx)
                | [| _; bady; Atom(Parser.Identifier _) |] ->
                    Error(UnexpectedAtom bady)
                | _ ->
                    Error(UnexpectedAtom args.[2])

        let registerSymbolList iatom body =
            let rec inner (regs: ImmutableArray<_>.Builder) iatom errors body =
                match body with
                | Atom(Parser.Identifier rname) :: remaining ->
                    let rname' = Name.ofStr rname
                    let errors' =
                        match registers.FromSymbol rname' with
                        | ValueSome r ->
                            regs.Add r
                            errors
                        | ValueNone -> SymbolNotDefined(rname', iatom) :: errors
                    inner regs iatom errors' remaining
                | bad :: remaining ->
                    inner regs iatom (UnexpectedAtom bad :: errors) remaining
                | [] -> struct(regs.ToImmutable(), errors)
            inner (ImmutableArray.CreateBuilder()) iatom errors body

        let (|CodeRegister|_|) body =
            keywordWithSymbol
                "register"
                (fun symbol iatom atoms errors state ->
                    match atoms with
                    | Atom(Parser.Identifier rt) :: extra -> // TODO: Allow parsing of register flags.
                        let rt' = Name.ofStr rt
                        match state.ModuleTypeSignatures.FromSymbol rt' with
                        | ValueSome rt'' ->
                            match registers.Add(symbol, { RegisterType = rt''; RegisterFlags = RegisterFlags.None }) with
                            | None -> [], extraneous extra errors
                            | Some err -> [], [ DuplicateRegisterSymbol(err iatom) ]
                        | ValueNone ->
                            [], [ SymbolNotDefined(rt', iatom) ]
                    | extra -> [], extraneous extra errors)
                body
                errors
                state

        match body with
        | [] ->
            match errors with
            | [] ->
                { Code.RegisterTypes =
                    let rtypes = ImmutableArray.CreateBuilder<struct(_ * _)>(registers.Count)
                    // TODO: Optimize by increasting count uint32 for registers instead of adding them one to one.
                    for register in registers do rtypes.Add(1u, register)
                    rtypes.ToImmutable()
                  Instructions = instrs.ToImmutable() }
                |> Ok
            | _ -> Error errors
        | Atom(Parser.Keyword "nop") :: body' ->
            instrs.Add InstructionSet.Nop
            inner body' errors
        | Atom(Parser.NestedAtom(Atom(Parser.Keyword "nop") :: extra)) :: body' ->
            inner body' (extraneous extra errors)
        | Atom(Parser.Keyword "ret") :: body' ->
            instrs.Add(InstructionSet.Ret ImmutableArray.Empty)
            inner body' errors
        | (Atom(Parser.NestedAtom(Atom(Parser.Keyword "ret") :: rregisters)) as iatom) :: body' ->
            let struct(regs, errors') = registerSymbolList iatom rregisters
            instrs.Add(InstructionSet.Ret regs)
            inner body' errors'

        | CodeRegister(body', errors)
        | Op3 "add" InstructionSet.Add (ValueSome(body', errors)) ->
            inner body' errors

        //| Op1 "incr"

        | (Atom(Parser.Keyword unknown) as op) :: body'
        | (Atom(Parser.NestedAtom(Atom(Parser.Keyword unknown) :: _)) as op) :: body' ->
            inner body' (UnknownInstruction(unknown, op) :: errors)
        | bad :: body' ->
            inner body' (InvalidInstruction bad :: errors)

    inner body errors

let (|ParsedVisibility|) flags contents =
    let inline setVisibilityFlags vis vatom contents' =
        match flags.contents with
        | ValueNone ->
            flags.contents <- ValueSome vis
            ValueSome(None, contents')
        | ValueSome _ ->
            ValueSome(Some(UnexpectedAtom vatom), contents')

    match contents with
    | (Atom(Parser.Keyword "visibility")) as vatom :: contents' ->
        setVisibilityFlags VisibilityFlags.Unspecified vatom contents'
    | (Atom(Parser.NestedAtom(Atom(Parser.Keyword "visibility") :: flags)) as vatom) :: contents' ->
        let inline ok flags = setVisibilityFlags flags vatom contents'
        match flags with
        | [ Atom(Parser.Keyword "public") ] -> ok VisibilityFlags.Public
        | [ Atom(Parser.Keyword "private") ] -> ok VisibilityFlags.Private
        | [ Atom(Parser.Keyword "unspecified") ] -> ok VisibilityFlags.Unspecified
        | _ -> ValueSome(Some(InvalidVisibility vatom), contents')
    | _ ->
        ValueNone

let (|ParsedName|) rname errors state atoms: struct(_ * _) voption =
    match atoms with
    | Atom(Parser.NestedAtom(Atom(Parser.Keyword "name") :: Atom(Parser.Identifier name') :: extra)) as natom :: remaining ->
        match rname.contents with
        | ValueNone ->
            let name' = Name.ofStr name'
            match state.ModuleIdentifiers.FromSymbol name' with
            | ValueSome namei ->
                rname.contents <- ValueSome namei
                ValueSome(remaining, extraneous extra errors)
            | ValueNone ->
                ValueSome(remaining, SymbolNotDefined(name', natom) :: errors)
        | ValueSome _ -> ValueSome(remaining, UnexpectedAtom natom :: errors)
    | _ -> ValueNone

let (|ParsedMethod|) errors state contents: struct(_ * _) voption =
    match contents with
    | (Atom(Parser.NestedAtom(Atom(Parser.Keyword "method") :: contents)) as matom) :: remaining ->
        // TODO: Avoid code duplication with type definition symbol
        let symbol, contents' =
            match contents with
            | Atom(Parser.Identifier symbol) :: contents' -> ValueSome(Name.ofStr symbol), contents'
            | _ -> ValueNone, contents

        let mutable mflags, msig, mkind = MethodFlags.Final, ValueNone, ValueNone
        let mname, vflags = ref ValueNone, ref ValueNone

        let rec inner contents errors =
            match contents with
            | ParsedName mname errors state (ValueSome(remaining, errors')) ->
                inner remaining errors'
            | ParsedVisibility vflags (ValueSome(result, remaining)) ->
                match result with
                | None -> inner remaining errors
                | Some err -> inner remaining (err :: errors)
            // TODO: Have loop to parse flags.
            // TODO: Parse signature + identifier to refer to signature
            | Atom(Parser.NestedAtom [ Atom(Parser.Keyword "signature"); Atom(Parser.Identifier sigid) ]) as satom :: remaining ->
                let sigid' = Name.ofStr sigid
                match msig, state.ModuleMethodSignatures.FromSymbol sigid' with
                | ValueNone, ValueSome sigi ->
                    msig <- ValueSome sigi
                    inner remaining errors
                | _ ->
                    inner remaining (UnexpectedAtom satom :: errors)
            // TODO: Parse method type + identifier to refer to method body
            | Atom(Parser.NestedAtom [ Atom(Parser.Keyword "defined"); Atom(Parser.Identifier bodyid) ]) as mkind' :: remaining ->
                let bodyid' = Name.ofStr bodyid
                match mkind, state.ModuleCode.FromSymbol bodyid' with
                | ValueNone, ValueSome bodyi ->
                    mkind <- ValueSome(MethodBody.Defined bodyi)
                    inner remaining errors
                | _ ->
                    inner remaining (UnexpectedAtom mkind' :: errors)
            | bad :: remaining ->
                inner remaining (UnexpectedAtom bad :: errors)
            | [] -> errors

        let errors' = inner contents' errors

        match msig, mkind with
        | ValueSome msig', ValueSome mbody' ->
            let method =
                { Method.MethodName =
                    match mname.contents with
                    | ValueSome tnamei -> tnamei
                    | ValueNone -> state.ModuleIdentifiers.AddAnonymous String.Empty
                  MethodVisibility = ValueOption.defaultValue VisibilityFlags.Private vflags.contents
                  MethodFlags = mflags
                  TypeParameters = ImmutableArray.Empty
                  Signature = msig'
                  MethodAnnotations = ImmutableArray.Empty
                  Body = mbody' }

            match state.ModuleMethods.AddDefinition(symbol, method) with
            | None -> ValueSome(remaining, errors')
            | Some err -> ValueSome(remaining, DuplicateMethodSymbol(err matom) :: errors')
        | ValueNone, _ ->
            ValueSome(remaining, MissingMethodSignature matom :: errors')
        | _, ValueNone ->
            ValueSome(remaining, MissingMethodKind matom :: errors')
    | _ -> ValueNone

let rec (|ModuleTypeDefinition|) errors state tatom: struct(TypeDefinition voption * _) voption =
    match tatom with
    | Atom(Parser.NestedAtom(Atom(Parser.Keyword "type") :: contents)) ->
        // TODO: Avoid code duplication with method definition symbol
        let symbol, contents' =
            match contents with
            | Atom(Parser.Identifier symbol) :: contents' -> ValueSome(Name.ofStr symbol), contents'
            | _ -> ValueNone, contents

        let mutable tkind = ValueNone
        let tname, vflags = ref ValueNone, ref ValueNone
        let fields, methods = ImmutableArray.CreateBuilder(), ImmutableArray.CreateBuilder()

        let rec inner contents errors =
            match contents with
            | ParsedName tname errors state (ValueSome(remaining, errors')) ->
                inner remaining errors'
            | ParsedVisibility vflags (ValueSome(result, remaining)) ->
                match result with
                | None -> inner remaining errors
                | Some err -> inner remaining (err :: errors)
            | Atom(Parser.NestedAtom(Atom(Parser.Keyword "class") :: attributes)) as katom :: remaining ->
                match tkind with
                | ValueNone ->
                    match attributes with
                    // TODO: Account for other type kind flags for classes
                    | [ Atom(Parser.Keyword "base"); Atom(Parser.Keyword "abstract") ] ->
                        tkind <- ValueSome(TypeDefinitionKind.Class(ValueNone, ClassDefinitionFlags.Abstract))
                        inner remaining errors
                    | _ ->
                        inner remaining (MissingTypeKind katom :: errors)
                | ValueSome _ ->
                    inner remaining (UnexpectedAtom katom :: errors)
            //| ParsedField
            //| ParsedMethod errors state (ValueSome(method, contents', errors')) ->
            //    ValueOption.iter methods.Add method
            //    inner contents' errors'
            | Atom(Parser.NestedAtom [ Atom(Parser.Keyword "method"); Atom(Parser.Identifier mname) ]) as matom :: remaining ->
                let mname' = Name.ofStr mname
                match state.ModuleMethods.FindDefinition mname' with
                | ValueSome mi ->
                    methods.Add mi
                    inner remaining errors
                | ValueNone ->
                    inner remaining (SymbolNotDefined(mname', matom) :: errors)
            | bad :: remaining ->
                inner remaining (UnexpectedAtom bad :: errors)
            | [] -> errors

        let errors' = inner contents' errors

        match tkind with
        | ValueSome tkind' ->
            let tdef =
                { TypeDefinition.TypeName =
                    match tname.contents with
                    | ValueSome tnamei -> tnamei
                    | ValueNone -> state.ModuleIdentifiers.AddAnonymous String.Empty
                  TypeVisibility = ValueOption.defaultValue VisibilityFlags.Unspecified vflags.contents
                  TypeKind = tkind'
                  TypeLayout = TypeDefinitionLayout.Unspecified // TODO: Implement parsing of (layout flags)
                  ImplementedInterfaces = ImmutableArray.Empty
                  TypeParameters = ImmutableArray.Empty
                  TypeAnnotations = ImmutableArray.Empty
                  Fields = fields.ToImmutable()
                  Methods = methods.ToImmutable() }

            match state.ModuleTypes.AddDefinition(symbol, tdef) with
            | None -> ValueSome(ValueSome tdef, errors')
            | Some err -> ValueSome(ValueNone, DuplicateTypeSymbol(err tatom) :: errors')
        | ValueNone ->
            ValueSome(ValueNone, errors')
    | Atom(Parser.NestedAtom(Atom(Parser.Keyword "type") :: _))
    | Atom(Parser.Keyword "type") ->
        failwith "TODO: Error for bad type (empty)"
    | _ -> ValueNone

let assemble atoms =
    let state =
        { ModuleFormatVersion = ref ValueNone
          ModuleHeaderName = ValueNone
          ModuleHeaderVersion = ref ValueNone
          ModuleIdentifiers = SymbolDictionary()
          ModuleTypeSignatures = SymbolDictionary()
          ModuleMethodSignatures = SymbolDictionary()
          ModuleCode = SymbolDictionary()
          ModuleNamespaces = NamespaceLookup()
          ModuleTypes = TypeDefinitionLookup()
          ModuleMethods = MethodLookup()
          ModuleEntryPoint = ref ValueNone }

    match atoms with
    | Atom(Parser.Keyword "module") :: contents ->
        let rec inner atoms errors =
            let inline (|VersionField|_|) name field err atoms =
                keyword
                    name
                    (fun fvatom atoms errors state ->
                        match field.contents with
                        | ValueNone -> versionNumberVector field atoms errors state
                        | ValueSome _ -> struct([], err fvatom :: errors))
                    atoms
                    errors
                    state

            let inline (|ModuleIdentifier|_|) atoms =
                keywordWithSymbol
                    "identifier"
                    (fun symbol iatom atoms errors state ->
                        match atoms with
                        | ParsedIdentifier(ValueSome name) :: extra ->
                            match state.ModuleIdentifiers.Add(symbol, string name) with
                            | None -> struct([], extraneous extra errors)
                            | Some err -> struct([], DuplicateIdentifierSymbol(err iatom) :: errors)
                        | ParsedIdentifier ValueNone :: _
                        | [] -> struct([], EmptyIdentifierEntry iatom :: errors))
                    atoms
                    errors
                    state

            let inline (|ModuleSignature|_|) atoms =
                keywordWithSymbol
                    "signature"
                    (fun symbol satom atoms errors state ->
                        match atoms with
                        | Atom(Parser.NestedAtom(Atom(Parser.Keyword "type") :: ParsedTypeSignature t)) as tatom :: extra ->
                            match t with
                            | ValueSome([], t') ->
                                match state.ModuleTypeSignatures.Add(symbol, t') with
                                | None -> [], extraneous extra errors
                                | Some err -> [], DuplicateSignatureSymbol(err satom) :: errors
                            | ValueSome(_ :: _, _)
                            | ValueNone ->
                                [], InvalidTypeSignature tatom :: errors
                        | Atom(Parser.NestedAtom(Atom(Parser.Keyword "method") :: msig)) as matom :: extra ->
                            match msig with
                            | ParsedMethodSignature matom errors state (Ok msig') ->
                                match state.ModuleMethodSignatures.Add(symbol, msig') with
                                | None -> [], extraneous extra errors
                                | Some err -> [], DuplicateSignatureSymbol(err satom) :: errors
                            | _ -> [], InvalidMethodSignature matom :: errors
                        | _ -> [], InvalidSignatureKind satom :: errors)
                    atoms
                    errors
                    state

            let inline (|ModuleCode|_|) atoms =
                keywordWithSymbol
                    "code"
                    (fun symbol catom body errors state ->
                        match instructions body errors state with
                        | Ok code ->
                            match state.ModuleCode.Add(symbol, code) with
                            | None -> [], errors
                            | Some err -> [], DuplicateCodeSymbol(err catom) :: errors
                        | Error errors' -> [], errors')
                    atoms
                    errors
                    state

            let inline (|ModuleNamespace|_|) atoms =
                keyword
                    "namespace"
                    (fun natom types errors state ->
                        let mutable nname = ValueNone
                        let types' = ImmutableArray.CreateBuilder()

                        let rec inner types errors =
                            match types with
                            | Atom(Parser.Keyword "name") :: remaining ->
                                nname <- ValueSome ImmutableArray.Empty
                                inner remaining errors
                            | (Atom(Parser.NestedAtom(Atom(Parser.Keyword "name") :: names)) as natom) :: remaining ->
                                let rec nloop (builder: ImmutableArray<_>.Builder) names =
                                    match names with
                                    | Atom(Parser.Identifier ni) :: names' ->
                                        let ni' = Name.ofStr ni
                                        match state.ModuleIdentifiers.FromSymbol ni' with
                                        | ValueSome n ->
                                            builder.Add n
                                            nloop builder names'
                                        | ValueNone ->
                                            Some(SymbolNotDefined(ni', natom))
                                    | bad :: _ ->
                                        Some(UnexpectedAtom bad)
                                    | [] ->
                                        nname <- ValueSome(builder.ToImmutable())
                                        None

                                match nloop (ImmutableArray.CreateBuilder()) names with
                                | None -> inner remaining errors
                                | Some err -> inner remaining (err :: errors)
                            | ModuleTypeDefinition errors state (ValueSome(t, errors')) :: remaining ->
                                ValueOption.iter types'.Add t
                                inner remaining errors'
                            | bad :: remaining ->
                                inner remaining (UnexpectedAtom bad :: errors)
                            | [] -> struct([], errors)

                        inner types errors)
                    atoms
                    errors
                    state

            match atoms with
            | VersionField "format" state.ModuleFormatVersion DuplicateFormatVersion (atoms', errors')
            | VersionField "version" state.ModuleHeaderVersion DuplicateModuleVersion (atoms', errors')
            | ModuleIdentifier(atoms', errors')
            | ModuleSignature(atoms', errors')
            | ModuleCode(atoms', errors')
            | ModuleNamespace(atoms', errors')
            | ParsedMethod errors state (ValueSome(atoms', errors')) ->
                inner atoms' errors'
            | Atom(Parser.NestedAtom(Atom(Parser.Keyword "name") as n :: ParsedIdentifier(ValueSome name) :: extra)) :: atoms' ->
                match state.ModuleHeaderName with
                | ValueNone ->
                    state.ModuleHeaderName <- ValueSome name
                    inner atoms' (extraneous extra errors)
                | ValueSome _ ->
                    inner atoms' (DuplicateModuleName n :: errors)
            | Atom(Parser.Keyword "name") as name :: atoms'
            | Atom(Parser.NestedAtom(Atom(Parser.Keyword "name") as name :: _)) :: atoms' ->
                inner atoms' (MissingModuleName(ValueSome name) :: errors)
            | Atom(Parser.NestedAtom(Atom(Parser.Keyword "entrypoint") :: Atom(Parser.Identifier epoint) :: extra)) as eatom :: atoms' ->
                let epoint' = Name.ofStr epoint
                match state.ModuleEntryPoint.contents, state.ModuleMethods.FindDefinition epoint' with
                | ValueNone, ValueSome entryi ->
                    state.ModuleEntryPoint.contents <- ValueSome entryi
                    inner atoms' (extraneous extra errors)
                | _ ->
                    inner atoms' (DuplicateEntryPoint eatom :: errors)
            | Atom(Parser.NestedAtom(Atom(Parser.Keyword "entrypoint") :: _)) as eatom :: atoms' ->
                inner atoms' (InvalidEntryPoint eatom :: errors)
            | unknown :: remaining ->
                inner remaining (UnexpectedAtom unknown :: errors)
            | [] -> errors

        let errors' = inner contents List.empty

        match state.ModuleHeaderName, errors' with
        | ValueSome mname, [] ->
            { Module.Magic = magic
              FormatVersion = VersionNumbers.ofValueOption state.ModuleFormatVersion.contents
              Header =
                { ModuleHeader.Module =
                    { ModuleIdentifier.ModuleName = mname
                      Version = VersionNumbers.ofValueOption state.ModuleHeaderVersion.contents }
                  Flags = ModuleHeaderFlags.LittleEndian
                  PointerSize = PointerSize.Unspecified }
              Identifiers = { IdentifierSection.Identifiers = state.ModuleIdentifiers.ToVector() }
              Imports = ImmutableArray.Empty
              TypeSignatures = state.ModuleTypeSignatures.ToVector()
              MethodSignatures = state.ModuleMethodSignatures.ToVector()
              Data = ImmutableArray.Empty
              Code = state.ModuleCode.ToVector()
              Fields = ImmutableArray.Empty // state.ModuleFields.ToVector()
              Methods = state.ModuleMethods.ToVector()
              Namespaces = ImmutableArray.CreateRange state.ModuleNamespaces
              EntryPoint = state.ModuleEntryPoint.contents
              Debug = () }
            |> Ok
        | ValueNone, errors ->
            if state.ModuleHeaderName.IsSome then failwith "uh oh"
            Error(MissingModuleName ValueNone :: errors)
        | _, (_ :: _ as errors) ->
            Error errors
    | _ ->
        Error [ ExpectedKeyword("module", List.tryHead atoms) ]
