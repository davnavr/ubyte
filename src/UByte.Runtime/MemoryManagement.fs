module UByte.Runtime.MemoryManagement

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.InteropServices

open Microsoft.FSharp.NativeInterop

#nowarn "9"

[<Struct>]
type ObjectType = ObjectType of uint32

[<Struct>]
type ObjectReference =
    val Address : nativeint

    new (addr) = { Address = addr }

    member o.IsNull = o.Address = 0n

    static member Null = Unchecked.defaultof<ObjectReference>

    override o.ToString() = sprintf "0x%08X" o.Address

let inline (|ObjectReference|) (o: ObjectReference) = o.Address

[<RequireQualifiedAccess>]
module ObjectReference =
    let inline toBytePtr (ObjectReference addr) = NativePtr.ofNativeInt<byte> addr
    let inline toVoidPtr o = NativePtr.toVoidPtr(toBytePtr o)

    [<RequiresExplicitTypeArguments>]
    let header<'Header when 'Header : unmanaged> (ObjectReference addr) =
        NativePtr.ofNativeInt<'Header>(addr - nativeint sizeof<'Header>)

type IGarbageCollector =
    abstract Allocate : ObjectType * size: int32 -> ObjectReference
    abstract Collect : (IGarbageCollector -> ObjectReference -> ImmutableArray<ObjectReference>) -> unit
    abstract TypeOf : ObjectReference -> ObjectType
    abstract Roots : ICollection<ObjectReference>

[<RequiresExplicitTypeArguments>]
let allocate<'Header when 'Header : unmanaged> size =
    if size < 0 then raise(ArgumentOutOfRangeException(nameof size, size, "Cannot allocate an object with negative size"))
    let hsize = sizeof<'Header>
    let addr = Marshal.AllocHGlobal(hsize + size)
    struct(NativePtr.ofNativeInt<'Header> addr, ObjectReference(addr + nativeint hsize))

[<RequiresExplicitTypeArguments>]
let free<'Header when 'Header : unmanaged> (ObjectReference addr) =
    Marshal.FreeHGlobal(addr - nativeint sizeof<'Header>)

let inline (|ByReference|) addr = NativePtr.toByRef addr

[<Flags>]
type MarkAndSweepFlags =
    | None = 0u
    | Marked = 1u

[<Struct; NoComparison; NoEquality>]
type MarkAndSweepHeader =
    { mutable Flags: MarkAndSweepFlags
      Type: ObjectType
      mutable Next: ObjectReference }

[<Sealed>]
type NaiveMarkAndSweep (threshold: uint32) =
    let mutable first = ObjectReference.Null
    let mutable last = ObjectReference.Null
    let mutable allocated = 0u
    let mutable threshold = threshold
    let roots = HashSet()
    let unmarked = Stack()

    static member ObjectFlags o =
        let (ByReference header) = ObjectReference.header<MarkAndSweepHeader> o
        &header.Flags

    static member NextObject o =
        let (ByReference header) = ObjectReference.header<MarkAndSweepHeader> o
        &header.Next

    member gc.Collect getReferencedObjects =
        // TODO: Take a lock before collection
        failwith "BAD"

        // Mark
        unmarked.Clear()
        for root in roots do unmarked.Push root

        while unmarked.Count > 0 do
            let o = unmarked.Pop()
            let oflags = &NaiveMarkAndSweep.ObjectFlags o
            oflags <- oflags ||| MarkAndSweepFlags.Marked
            for referenced in getReferencedObjects gc o do
                if NaiveMarkAndSweep.ObjectFlags referenced &&& MarkAndSweepFlags.Marked = MarkAndSweepFlags.None then
                    unmarked.Push referenced

        // Sweep
        let mutable current, previous = first, ObjectReference.Null
        while not current.IsNull do
            let (ByReference header) = ObjectReference.header<MarkAndSweepHeader> current

            if header.Flags &&& MarkAndSweepFlags.Marked = MarkAndSweepFlags.None then
                if current = first then first <- header.Next
                free<MarkAndSweepHeader> current
                if not previous.IsNull then NaiveMarkAndSweep.NextObject previous <- header.Next

            header.Flags <- header.Flags &&& (~~~MarkAndSweepFlags.Marked)
            if header.Next.IsNull then last <- current
            previous <- current
            current <- header.Next

    interface IGarbageCollector with
        member _.Roots = roots :> ICollection<_>

        member gc.Collect getReferencedObjects = gc.Collect getReferencedObjects

        member gc.Allocate(ty, size) =
            // TODO: Take a lock before allocation
            let struct(ByReference header, o) = allocate<MarkAndSweepHeader> size
            allocated <- allocated + uint32 size
            header <- { Flags = MarkAndSweepFlags.None; Type = ty; Next = ObjectReference.Null }

            if allocated > threshold then
                failwith "TODO: Make getReferencedObjects function be a field so gc.Collect() can be used in Allocate function"
                threshold <- allocated

            if first.IsNull then first <- o
            if not last.IsNull then NaiveMarkAndSweep.NextObject last <- o
            last <- o
            o

        member _.TypeOf o =
            let (ByReference header) = ObjectReference.header<MarkAndSweepHeader> o
            header.Type

[<AbstractClass; Sealed>]
type CollectionStrategies =
    static member NaiveMarkAndSweep threshold = NaiveMarkAndSweep threshold :> IGarbageCollector
    static member NaiveMarkAndSweep() = CollectionStrategies.NaiveMarkAndSweep(0xFFFu)
