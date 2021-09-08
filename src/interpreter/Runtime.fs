module UByte.Interpreter.Runtime

open System.Collections.Generic
open System.IO

open UByte.Format
open UByte.Format.Model

[<Sealed>]
type RuntimeModule (m: Module) =
    member _.Module = m

let executionEntryPoint (program: Module) (moduleReferenceResolver: ModuleImport -> Module) =
    let resolvedModuleImports = Dictionary<ModuleImport, RuntimeModule> program.Imports.Length

    match program.EntryPoint with
    | ValueSome main ->

        ()
    | ValueNone -> failwithf "TODO: Error for missing entry point method"

let run program = executionEntryPoint (ParseModule.fromFile program) (fun _ -> failwith "TODO: Implement resolution of references")
