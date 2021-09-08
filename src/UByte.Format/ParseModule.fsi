[<RequireQualifiedAccess>]
module UByte.Format.ParseModule

open System.IO

//val fromSeq : source: seq<byte> -> Format.Module

/// <exception cref="T:System.ArgumentNullException">
/// Thrown when the <paramref name="source"/> is <see langword="null"/>.
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
