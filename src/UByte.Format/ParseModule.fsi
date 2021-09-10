/// Contains functions for parsing modules, without validating the parsed contents.
[<RequireQualifiedAccess>]
module UByte.Format.ParseModule

open System.IO

//val fromSeq : source: seq<byte> -> Format.Module

/// <exception cref="T:System.ArgumentNullException">
/// Thrown when the <paramref name="source"/> is <see langword="null"/>.
/// </exception>
/// <exception cref="T:System.ArgumentException">
/// Thrown when the <paramref name="source"/> does not support reading.
/// </exception>
val fromStream : source: Stream -> Model.Module

/// <exception cref="T:System.ArgumentNullException">
/// Thrown when the <paramref name="source"/> is <see langword="null"/>.
/// </exception>
val fromFile : source: FileInfo -> Model.Module

/// <exception cref="T:System.ArgumentNullException">
/// Thrown when the <paramref name="source"/> is <see langword="null"/>.
/// </exception>
val fromPath : source: string -> Model.Module
