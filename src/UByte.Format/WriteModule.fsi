[<RequireQualifiedAccess>]
module UByte.Format.WriteModule

open System.IO

/// <exception cref="T:System.ArgumentNullException">
/// Thrown when the <paramref name="stream"/> is <see langword="null"/>.
/// </exception>
val toStream : stream: Stream -> ``module``: Format.Module -> unit

/// <exception cref="T:System.ArgumentNullException">
/// Thrown when the <paramref name="destination"/> file is <see langword="null"/>.
/// </exception>
val toFile : destination: FileInfo -> ``module``: Format.Module -> unit

/// <exception cref="T:System.ArgumentNullException">
/// Thrown when the <paramref name="destination"/> is <see langword="null"/>.
/// </exception>
val toPath : destination: string -> ``module``: Format.Module -> unit
