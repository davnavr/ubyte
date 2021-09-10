/// Contains functions for writing modules, without validation.
[<RequireQualifiedAccess>]
module UByte.Format.WriteModule

open System.IO

/// <exception cref="T:System.ArgumentNullException">
/// Thrown when the <paramref name="stream"/> is <see langword="null"/>.
/// </exception>
/// <exception cref="T:System.ArgumentException">
/// Thrown when the <paramref name="stream"/> does not support writing.
/// </exception>
val toStream : stream: Stream -> md: Model.Module -> unit

/// <exception cref="T:System.ArgumentNullException">
/// Thrown when the <paramref name="destination"/> file is <see langword="null"/>.
/// </exception>
val toFile : destination: FileInfo -> md: Model.Module -> unit

/// <exception cref="T:System.ArgumentNullException">
/// Thrown when the <paramref name="destination"/> is <see langword="null"/>.
/// </exception>
val toPath : destination: string -> md: Model.Module -> unit
