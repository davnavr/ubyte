module UByte.Assembler.Assemble

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FParsec

open UByte.Format.Model

let emptyIdentifierIndex = IdentifierIndex.Index 0u

type AssemblerError =
    | ParserError of string * ParserError
    | ValidationError of string * Position

    override this.ToString() =
        match this with
        | ParserError(msg, _)
        | ValidationError(msg, _) -> msg

// TODO: Keep name of SymbolDictionary

type IncompleteModule =
    { ModuleFormatVersion: VersionNumbers voption ref
      mutable ModuleHeaderName: Name voption
      ModuleHeaderVersion: VersionNumbers voption ref
      mutable ModuleEntryPoint: (Position * Name) voption }

[<Struct>]
type State = { Module: IncompleteModule; Errors: AssemblerError list }

type AssemblerResult = Result<Module, AssemblerError list>

[<Sealed>]
type AssemblerResultBuilder () =
    member inline _.Bind(result: Result<_, AssemblerError list>, body: 'T -> Result<'U, _>) =
        match result with
        | Result.Ok result' -> body result'
        | Result.Error err -> Result.Error err

    member inline _.Return value = Result<_, AssemblerError list>.Ok value

    member inline _.ReturnFrom(result: Result<_, AssemblerError list>) = result

let validated = AssemblerResultBuilder()

let fromInput input: AssemblerResult =
    match input Parser.declarations with
    | Success(declarations: Parser.ParsedDeclaration list, (), _) ->
        failwith "TODO: Parsing was successful"
    | Failure(msg, err, ()) ->
        Result.Error [ ParserError(msg, err) ]
