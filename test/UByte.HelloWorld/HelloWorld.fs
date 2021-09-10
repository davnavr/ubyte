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
                    struct(1u, { RegisterType = Index 0u; RegisterFlags = RegisterFlags.None })
                |]
              Instructions =
                ImmutableArray.CreateRange [|
                    // TODO: Do some loading constant and adding stuff here
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

let tests = testList "hello world" [
    testCase "writes to file" <| fun () ->
        WriteModule.toPath "hello_world.mdle" program

    testCase "written to stream can be parsed" <| fun () ->
        let wbuffer = new MemoryStream()
        WriteModule.toStream wbuffer program
        let rbuffer = new MemoryStream(wbuffer.ToArray())
        let parsed = ParseModule.fromStream rbuffer
        parsed.Header.Module =! program.Header.Module
]

[<EntryPoint>]
let main argv =
    runTestsWithCLIArgs Seq.empty argv tests
