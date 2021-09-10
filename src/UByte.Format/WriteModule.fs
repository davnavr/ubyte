module UByte.Format.WriteModule

open System
open System.IO

open UByte.Format.Model
open UByte.Format.Model.InstructionSet

let inline bits1 (value: 'Enum when 'Enum : enum<uint8>) (dest: Stream) = dest.WriteByte(uint8 value)

[<RequireQualifiedAccess>]
module LEB128 =
    let [<Literal>] private ContinueMask = 0b1000_0000uy

    let inline private unsigned value (dest: Stream) =
        if value <> LanguagePrimitives.GenericZero then
            let mutable value = value
            while value > LanguagePrimitives.GenericZero do
                let b = uint8 value &&& (~~~ContinueMask)
                value <- value >>> 7;
                let cont = if value > LanguagePrimitives.GenericZero then ContinueMask else 0uy
                dest.WriteByte(b ||| cont)
        else
            dest.WriteByte 0uy

    let uint value dest = unsigned value dest

let inline index (Index i) dest = LEB128.uint i dest

let vector writer (items: vector<'T>) dest =
    LEB128.uint (uint32 items.Length) dest
    for i = 0 to items.Length - 1 do
        writer items.[i] dest

let versions (VersionNumbers numbers) dest = vector LEB128.uint numbers dest

let name (Name name) (dest: Stream) =
    // NOTE: Can write strings more efficeintly by using StreamWriter or Encoder
    dest.Write(ReadOnlySpan(System.Text.Encoding.UTF8.GetBytes name))

let lengthEncodedData (buffer: MemoryStream) dest writer =
    let inline reset() = buffer.Seek(0L, SeekOrigin.Begin) |> ignore

    buffer.SetLength 0L
    reset()
    writer buffer

    LEB128.uint (uint32 buffer.Length) dest
    buffer.WriteTo dest

let lengthEncodedVector buffer dest items writer = lengthEncodedData buffer dest (vector writer items)

let moduleID id dest =
    name id.ModuleName dest
    versions id.Version dest

let fieldDef f dest =
    index f.FieldName dest
    bits1 f.FieldVisibility dest
    bits1 f.FieldFlags dest
    index f.FieldType dest
    if not f.FieldAnnotations.IsDefaultOrEmpty then failwith "TODO: Annotated fields not yet supported"

let methodDef m dest =
    index m.MethodName dest
    bits1 m.MethodVisibility dest
    bits1 m.MethodFlags dest
    if not m.TypeParameters.IsDefaultOrEmpty then failwith "TODO: Generic methods not yet supported"
    index m.Signature dest
    if not m.MethodAnnotations.IsDefaultOrEmpty then failwith "TODO: Annotated methods not yet supported"
    index m.Body dest

let typeDef t dest =
    index t.TypeName dest
    bits1 t.TypeVisibility dest

    match t.TypeKind with
    | Class(extends, flags) ->
        bits1 Tag.TypeDefinitionKind.Class dest
        index extends dest
        bits1 flags dest
    | Interface ->
        bits1 Tag.TypeDefinitionKind.Interface dest
    | Struct ->
        bits1 Tag.TypeDefinitionKind.Struct dest

    match t.TypeLayout with
    | TypeDefinitionLayout.Unspecified ->
        bits1 Tag.TypeDefinitionLayout.Unspecified dest
    | TypeDefinitionLayout.Sequential ->
        bits1 Tag.TypeDefinitionLayout.Sequential dest

    if not t.ImplementedInterfaces.IsDefaultOrEmpty then failwith "TODO: Writing of implemented interfaces not yet supported"
    if not t.TypeParameters.IsDefaultOrEmpty then failwith "TODO: Generic types not yet supported"
    if not t.TypeAnnotations.IsDefaultOrEmpty then failwith "TODO: Annotated types not yet supported"

    vector fieldDef t.Fields dest
    vector methodDef t.Methods dest

let registerType (struct(count: uvarint, t: RegisterType)) dest =
    LEB128.uint count dest
    index t.RegisterType dest
    bits1 t.RegisterFlags dest

let inline opcode (code: Opcode) dest = LEB128.uint (uint32 code) dest

let instruction i dest =
    match i with
    | Instruction.Nop -> opcode Opcode.nop dest
    | Instruction.Ret registers ->
        opcode Opcode.ret dest
        vector index registers dest
    | Instruction.Call(method, aregs, rregs) ->
        opcode Opcode.call dest
        index method dest
        vector index aregs dest
        vector index rregs dest
    | Instruction.Reg_copy(sreg, dreg) ->
        opcode Opcode.``reg.copy`` dest
        index sreg dest
        index dreg dest
    | Instruction.Add(xreg, yreg, rreg) ->
        opcode Opcode.add dest
        index xreg dest
        index yreg dest
        index rreg dest
    | Instruction.Sub(xreg, yreg, rreg) ->
        opcode Opcode.sub dest
        index xreg dest
        index yreg dest
        index rreg dest
    | bad -> failwithf "TODO: Bad instr %A" bad

let toStream (stream: Stream) (md: Module) =
    if isNull stream then nullArg(nameof stream)
    if not stream.CanWrite then invalidArg (nameof stream) "The stream must support writing"
    try
        let buffer = new MemoryStream()
        let (Magic magic) = md.Magic
        stream.Write(magic.AsSpan())
        versions md.FormatVersion stream

        lengthEncodedData buffer stream <| fun dest ->
            let header = md.Header
            LEB128.uint header.FieldCount dest
            moduleID header.Module dest
            bits1 header.Flags dest
            bits1 header.PointerSize dest

        lengthEncodedVector buffer stream md.Identifiers.Identifiers name

        lengthEncodedVector buffer stream md.Imports <| fun import dest ->
            failwith "TODO: Imports not supported yet"

        lengthEncodedVector buffer stream md.TypeSignatures <| fun signature dest ->
            match signature with
            | Primitive prim ->
                let tag =
                    match prim with
                    | PrimitiveType.S8 -> Tag.Type.S8
                    | PrimitiveType.S16 -> Tag.Type.S16
                    | PrimitiveType.S32 -> Tag.Type.S32
                    | PrimitiveType.S64 -> Tag.Type.S64
                    | PrimitiveType.U8 -> Tag.Type.U8
                    | PrimitiveType.U16 -> Tag.Type.U16
                    | PrimitiveType.U32 -> Tag.Type.U32
                    | PrimitiveType.U64 -> Tag.Type.U64
                    | PrimitiveType.F32 -> Tag.Type.F32
                    | PrimitiveType.F64 -> Tag.Type.F64
                    | PrimitiveType.Char16 -> Tag.Type.Char16
                    | PrimitiveType.Char32 -> Tag.Type.Char32
                    | PrimitiveType.Bool -> Tag.Type.Bool
                    | PrimitiveType.Unit -> Tag.Type.Unit
                bits1 tag dest
            | _ -> failwithf "TODO: Unsupported type %A" signature

        lengthEncodedVector buffer stream md.MethodSignatures <| fun signature dest ->
            vector index signature.ReturnTypes dest
            vector index signature.ParameterTypes dest

        lengthEncodedVector buffer stream md.Data <| fun data dest ->
            failwith "TODO: Data not supported yet"

        lengthEncodedVector buffer stream md.Code <| fun code dest ->
            vector registerType code.RegisterTypes dest
            vector instruction code.Instructions dest

        lengthEncodedVector buffer stream md.Namespaces <| fun ns dest ->
            vector index ns.NamespaceName dest
            vector typeDef ns.TypeDefinitions dest
            if not ns.TypeAliases.IsDefaultOrEmpty then failwith "TODO: Type aliases not yet supported"

        match md.EntryPoint with
        | ValueNone -> LEB128.uint 0u stream
        | ValueSome main -> lengthEncodedData buffer stream (index main)

        // Debug information not yet supported
    finally
        stream.Close()

let toFile (destination: FileInfo) md =
    if isNull destination then nullArg(nameof destination)
    toStream (destination.OpenWrite()) md

let toPath destination md = toFile (FileInfo destination) md
