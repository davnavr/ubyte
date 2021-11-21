[<RequireQualifiedAccess>]
module MiddleC.Compiler.CodeGenerator

/// <exception cref="T:System.ArgumentException">Thrown when the module contains one or more errors.</exception>
val write : MiddleC.Compiler.Semantics.CheckedModule -> UByte.Format.Model.Module
