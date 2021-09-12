module HelloWorld

open System.Collections.Immutable
open System.IO

open Expecto

open Swensen.Unquote

open UByte.Format
open UByte.Format.Model
open UByte.Format.Model.InstructionSet

let program =
    { Magic = Model.magic
      FormatVersion = Model.currentFormatVersion
      Header =
        { Module = { ModuleName = Name.ofStr "HelloWorld"; Version = VersionNumbers.semver 0u 1u 0u }
          Flags = ModuleHeaderFlags.LittleEndian
          PointerSize = PointerSize.Unspecified }
      Identifiers =
        let identifiers =
            Array.map Name.ofStr [|
                "Example"
                "HelloWorld"
                "main"
            |]
        
        { IdentifierSection.Identifiers = ImmutableArray.Create(items = identifiers) }
      Imports = ImmutableArray.Empty
      TypeSignatures =
        ImmutableArray.CreateRange [|
            AnyType.Primitive PrimitiveType.S32
        |]
      MethodSignatures =
        ImmutableArray.CreateRange [|
            { ReturnTypes = ImmutableArray.Create(Index 0u)
              ParameterTypes = ImmutableArray.Empty }
        |]
      Data = ImmutableArray.Empty
      Code =
        ImmutableArray.CreateRange [|
            { RegisterTypes =
                ImmutableArray.CreateRange [|
                    struct(2u, { RegisterType = Index 0u; RegisterFlags = RegisterFlags.None })
                |]
              Instructions =
                ImmutableArray.CreateRange [|
                    Const_i32(9, Index 0u)
                    Const_i32(10, Index 1u)
                    Add(Index 0u, Index 1u, Index 0u)
                    Ret(ImmutableArray.Create(Index 0u))
                |] }
        |]
      Namespaces =
        ImmutableArray.CreateRange [|
            { NamespaceName = ImmutableArray.Create(Index 0u)
              TypeDefinitions =
                ImmutableArray.CreateRange [
                    { TypeName = Index 1u
                      TypeVisibility = VisibilityFlags.Public
                      TypeKind = TypeDefinitionKind.Class(ValueNone, ClassDefinitionFlags.Abstract)
                      TypeLayout = TypeDefinitionLayout.Unspecified
                      ImplementedInterfaces = ImmutableArray.Empty
                      TypeParameters = ImmutableArray.Empty
                      TypeAnnotations = ImmutableArray.Empty
                      Fields = ImmutableArray.Empty
                      Methods =
                        ImmutableArray.Create (
                            { MethodName = Index 2u
                              MethodVisibility = VisibilityFlags.Public
                              MethodFlags = MethodFlags.Static
                              TypeParameters = ImmutableArray.Empty
                              Signature = Index 0u
                              MethodAnnotations = ImmutableArray.Empty
                              Body = MethodBody.Defined(Index 0u) }
                        ) }
                ]
              TypeAliases = ImmutableArray.Empty }
        |]
      EntryPoint = ValueSome(Index 0u)
      Debug = () }

type private This = class end

let getEmbeddedText =
    let assem = typeof<This>.Assembly
    let names = assem.GetManifestResourceNames()
    fun (name: string) -> Array.find (fun (str: string) -> str.EndsWith name) names |> assem.GetManifestResourceStream

let tests = testList "hello world" [
    testCase "writes to file" <| fun () ->
        WriteModule.toPath "hello_world.mdle" program

    testCase "written to stream can be parsed" <| fun () ->
        let wbuffer = new MemoryStream()
        WriteModule.toStream wbuffer program
        let rbuffer = new MemoryStream(wbuffer.ToArray())
        let parsed = ParseModule.fromStream rbuffer
        parsed.Header.Module =! program.Header.Module

    let parseHelloWorld() =
        let result =
            FParsec.CharParsers.runParserOnStream
                UByte.Assembler.Parser.sexpression
                ()
                "HelloWorld"
                (getEmbeddedText "HelloWorld.tmodule")
                System.Text.Encoding.UTF8
        match result with
        | FParsec.CharParsers.ParserResult.Success(atoms, _, _) -> atoms
        | FParsec.CharParsers.ParserResult.Failure(msg, err, ()) -> failwithf "Parsing failed %A %A" msg err

    testCase "text format can be parsed" (parseHelloWorld >> ignore)

    testCase "text format can be assembled" <| fun() ->
        let atoms = parseHelloWorld()
        match UByte.Assembler.Assembler.assemble atoms with
        | Ok _ -> ()
        | Error errs ->
            let errors = System.Text.StringBuilder()
            for err in errs do errors.Append("Error: ").Append(err).AppendLine() |> ignore
            failtest(errors.ToString())
]

[<EntryPoint>]
let main argv =
    runTestsWithCLIArgs Seq.empty argv tests
