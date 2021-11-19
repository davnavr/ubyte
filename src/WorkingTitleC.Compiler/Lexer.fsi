namespace WorkingTitleC.Compiler.Lexer

open System
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

    override ToString : unit -> string

    interface IEquatable<TokenContent>

[<System.Runtime.CompilerServices.IsReadOnly; Struct; NoComparison; StructuralEquality>]
type Token =
    { Line: uint32
      Column: uint32
      Content: TokenContent }

    interface IEquatable<Token>

type TokenizedFile = ImmutableArray<Token>

[<RequireQualifiedAccess>]
module Lex =
    open System.IO

    val fromEnumerator : byref<#System.Collections.Generic.IEnumerator<Rune>> -> TokenizedFile

    /// <exception cref="T:System.ArgumentNullException"/>
    val fromFile : file: System.IO.FileInfo -> TokenizedFile

    /// <exception cref="T:System.ArgumentNullException"/>
    val fromStream : Stream -> TokenizedFile

    val fromPath : path: string -> TokenizedFile

    /// <exception cref="T:System.ArgumentNullException"/>
    val fromSeq : characters: seq<Rune> -> TokenizedFile
