namespace WorkingTitleC.Compiler.Lexer

open System.Collections.Immutable
open System.Text

[<NoComparison; StructuralEquality>]
type TokenContent =
    | Identifier of string
    | Comment of string
    | OpenParen
    | CloseParen
    | Semicolon
    | Unknown of ImmutableArray<Rune>

[<Struct; NoComparison; StructuralEquality>]
type Token = { Line: uint32; Column: uint32; Content: TokenContent }

type TokenizedFile = ImmutableArray<Token>

[<RequireQualifiedAccess>]
module Lex =
    open System
    open System.Collections.Generic
    open System.IO

    let fromEnumerator (enumerator: byref<#IEnumerator<Rune>>) =
        try
            let tokens = ImmutableArray.CreateBuilder()
            let unknownCharacterList = ImmutableArray.CreateBuilder()
            let mutable line, col = 0u, 0u

            let addTokenContent content =
                tokens.Add { Line = line; Column = col; Content = content }

            while enumerator.MoveNext() do
                match enumerator.Current.Value with
                | 0x40 -> addTokenContent OpenParen


                if enumerator.Current.Utf16SequenceLength = 1 then
                    let mutable commitUnknownCharacters = true

                    match char enumerator.Current.Value with
                    | ';' -> addTokenContent Semicolon
                    | '(' -> addTokenContent Semicolon
                    | ')' -> addTokenContent Semicolon
                    | _ ->
                        commitUnknownCharacters <- false
                        unknownCharacterList.Add enumerator.Current

                    if commitUnknownCharacters && unknownCharacterList.Count > 0 then
                        addTokenContent(TokenContent.Unknown(unknownCharacterList.ToImmutable()))
                        unknownCharacterList.Clear()
                else
                    unknownCharacterList.Add enumerator.Current

            tokens
        finally
            enumerator.Dispose()

    [<Struct; NoComparison; NoEquality>]
    type StreamRuneEnumerator =
        val private reader: StreamReader
        [<DefaultValue>] val mutable private current: Rune

        new (stream: Stream) = { reader = new StreamReader(stream) }

        member private chars.ReadChar() =
            match chars.reader.Read() with
            | -1 -> ValueNone
            | c -> ValueSome(char c)

        interface IEnumerator<Rune> with
            member chars.Current = chars.current

            member chars.MoveNext() =
                match chars.ReadChar() with
                | ValueSome c1 ->
                    if not(Char.IsSurrogate c1) then
                        chars.current <- Rune c1
                    else
                        match chars.ReadChar() with
                        | ValueSome c2 when Char.IsHighSurrogate c1 && Char.IsLowSurrogate c2 ->
                            chars.current <- Rune(c2, c1)
                        | ValueSome _ ->
                            failwith "TODO: Error for unexpected low surrogate"
                        | ValueNone ->
                            failwith "TODO: Error for missing surrogate pair."

                    true
                | ValueNone ->
                    false

            member chars.Dispose() = chars.reader.Close()

            member chars.Current = box chars.current

            member _.Reset() = raise(NotImplementedException())

    let fromStream stream =
        if isNull stream then nullArg (nameof stream)
        use mutable enumerator = new StreamRuneEnumerator(stream)
        fromEnumerator &enumerator

    let fromFile (file: FileInfo) =
        if isNull file then nullArg (nameof file)
        fromStream(file.OpenRead())

    let fromPath path = fromStream(File.OpenRead path)

    let fromSeq (characters: seq<_>) =
        if isNull characters then nullArg (nameof characters)
        let mutable enumerator = characters.GetEnumerator()
        fromEnumerator &enumerator
