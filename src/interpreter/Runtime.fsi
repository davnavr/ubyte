[<RequireQualifiedAccess>]
module UByte.Interpreter.Runtime

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO

open UByte.Format

[<Sealed>]
type RuntimeMethod = class end

[<Sealed>]
type RuntimeTypeDefinition =
    member Namespace: ImmutableArray<string>

    //member InvokeInitializer: unit -> 

[<Sealed>]
type RuntimeModule =
    member InitializeType: Model.TypeDefinitionIndex -> RuntimeTypeDefinition
    member InitializeMethod: Model.MethodIndex -> RuntimeMethod

    interface IEquatable<RuntimeModule>

type RuntimeMethod with member Module: RuntimeModule
type RuntimeTypeDefinition with member Module: RuntimeModule

val run: program: FileInfo -> importDirs: IReadOnlyCollection<DirectoryInfo> -> int32
