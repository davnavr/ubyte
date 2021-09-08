[<RequireQualifiedAccess>]
module internal UByte.Format.Span

open System

open Microsoft.FSharp.NativeInterop

#nowarn "9"

let inline stackalloc<'T when 'T : unmanaged> length = Span<'T>(NativePtr.toVoidPtr(NativePtr.stackalloc<'T> length), length)

let inline asReadOnly<'T> (m: Span<'T>) = Span.op_Implicit m
