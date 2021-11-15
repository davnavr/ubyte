module UByte.Runtime.MemoryManagement

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices
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
    let inline toNativePtr<'T when 'T : unmanaged> (ObjectReference addr) = NativePtr.ofNativeInt<'T> addr
    let inline toVoidPtr o = NativePtr.toVoidPtr(toNativePtr<byte> o)
    let inline toNativeInt(ObjectReference i) = i

    [<RequiresExplicitTypeArguments>]
    let getHeaderPtr<'Header when 'Header : unmanaged> (ObjectReference addr) =
        NativePtr.ofNativeInt<'Header>(addr - nativeint sizeof<'Header>)

    [<RequiresExplicitTypeArgumentsAttribute>]
    let getHeaderRef<'Header when 'Header : unmanaged> o = NativePtr.toByRef(getHeaderPtr<'Header> o)

type ObjectSizeLookup = ObjectType -> int32

type IGarbageCollector =
    inherit IDisposable

    abstract Allocate : ObjectType * size: int32 -> ObjectReference
    abstract Collect : ReferencedObjectsLookup * ObjectSizeLookup -> unit
    abstract TypeOf : ObjectReference -> ObjectType
    [<CLIEvent>] abstract Allocated : IEvent<struct(int32 * ObjectReference)>
    [<CLIEvent>] abstract Collected : IEvent<nativeint>

and ReferencedObjectsLookup = IGarbageCollector -> ObjectReference -> ImmutableArray<ObjectReference>

[<AbstractClass>]
type ValidatedCollector () =
    let mutable disposed = false
    let allocated = Event<_>()

    member val Collected = Event<_>()

    /// Frees all objects allocated by this collector, ensure that managed objects are not accessed as this may be called by the
    /// finalizer.
    abstract Deallocate : unit -> unit

    abstract Allocate : ObjectType * size: int32 -> ObjectReference

    abstract Collect : ReferencedObjectsLookup * ObjectSizeLookup -> unit

    abstract TypeOf : ObjectReference -> ObjectType

    member private gc.CheckDisposed() = if disposed then raise(ObjectDisposedException(nameof gc))

    interface IGarbageCollector with
        member gc.Dispose() =
            if not disposed then
                gc.Deallocate()
                disposed <- true
            GC.SuppressFinalize gc

        member gc.Allocate(ty, size) =
            gc.CheckDisposed()
            if size < 0 then
                raise(ArgumentOutOfRangeException(nameof size, size, "Cannot allocate an object with a negative size"))
            let o = gc.Allocate(ty, size)
            allocated.Trigger(struct(size, o))
            o

        member gc.Collect(orefs, osize) =
            gc.CheckDisposed()
            gc.Collect(orefs, osize)

        member gc.TypeOf o =
            gc.CheckDisposed()
            gc.TypeOf o

        [<CLIEvent>]
        member _.Allocated = allocated.Publish

        [<CLIEvent>]
        member gc.Collected = gc.Collected.Publish

/// Ensures that a lock on an object is acquired when an allocation or collection is made.
[<Sealed>]
type PausingCollector<'Collector when 'Collector :> IGarbageCollector> (collector: 'Collector) =
    let lobj = obj()

    interface IGarbageCollector with
        member _.Dispose() = collector.Dispose()
        member _.Allocate(ty, size) = lock lobj (fun() -> collector.Allocate(ty, size))
        member _.Collect(orefs, osize) = lock lobj (fun() -> collector.Collect(orefs, osize))
        member _.TypeOf o = collector.TypeOf o
        [<CLIEvent>] member _.Allocated = collector.Allocated
        [<CLIEvent>] member _.Collected = collector.Collected

[<RequiresExplicitTypeArguments>]
let alloch<'Header when 'Header : unmanaged> size =
    let hsize = sizeof<'Header>
    ObjectReference(Marshal.AllocHGlobal(hsize + size) + nativeint hsize)

[<RequiresExplicitTypeArguments>]
let freeh<'Header when 'Header : unmanaged> (ObjectReference addr) =
    Marshal.FreeHGlobal(addr - nativeint sizeof<'Header>)

[<Flags>]
type MarkAndSweepFlags =
    | None = 0u
    | Marked = 1u

[<Struct; NoComparison; NoEquality>]
type MarkAndSweepHeader =
    { mutable Flags: MarkAndSweepFlags
      Type: ObjectType
      mutable Next: ObjectReference }

let markAndSweepNext o =
    let header = &ObjectReference.getHeaderRef<MarkAndSweepHeader> o
    &header.Next

let markAndSweepFlags o=
   let header = &ObjectReference.getHeaderRef<MarkAndSweepHeader> o
   &header.Flags

[<Sealed>]
type MarkAndSweep (threshold: uint32) =
    inherit ValidatedCollector()

    let mutable first = ObjectReference.Null
    let mutable allocated = 0u
    let mutable threshold = threshold
    let roots = HashSet()
    let unmarked = Stack()

    override _.Allocate(ty, size) =
        let o = alloch<MarkAndSweepHeader> size

        let header = &ObjectReference.getHeaderRef<MarkAndSweepHeader> o
        header.Flags <- MarkAndSweepFlags.None
        Unsafe.AsRef &header.Type <- ty
        header.Next <- ObjectReference.Null

        if allocated > threshold then
            failwith "TODO: Make getReferencedObjects function be a field so gc.Collect() can be used in Allocate function"
            threshold <- allocated

        if first.IsNull then first <- o
        o

    override _.TypeOf o =
        let header: inref<_> = &ObjectReference.getHeaderRef<MarkAndSweepHeader> o
        header.Type

    member private gc.Free o =
        freeh<MarkAndSweepHeader> o
        gc.Collected.Trigger(ObjectReference.toNativeInt o)

    override gc.Collect(getReferencedObjects, _) =
        // Mark
        unmarked.Clear()
        for root in roots do unmarked.Push root

        while unmarked.Count > 0 do
            let o = unmarked.Pop()
            let oflags = &markAndSweepFlags o
            oflags <- oflags ||| MarkAndSweepFlags.Marked
            for referenced in getReferencedObjects gc o do
                if markAndSweepFlags referenced &&& MarkAndSweepFlags.Marked = MarkAndSweepFlags.None then
                    unmarked.Push referenced

        // Sweep
        let mutable current, previous = first, ObjectReference.Null
        while not current.IsNull do
            let header = &ObjectReference.getHeaderRef<MarkAndSweepHeader> current

            if header.Flags &&& MarkAndSweepFlags.Marked = MarkAndSweepFlags.None then
                if current = first then first <- header.Next
                gc.Free current
                if not previous.IsNull then markAndSweepNext previous <- header.Next

            header.Flags <- header.Flags &&& (~~~MarkAndSweepFlags.Marked)
            previous <- current
            current <- header.Next

    override gc.Deallocate() =
        let mutable current = first
        while not current.IsNull do
            let next = markAndSweepNext current
            gc.Free current
            current <- next

[<AbstractClass; Sealed>]
type GarbageCollectors =
    static member MarkAndSweep(?threshold) =
        new PausingCollector<_>(new MarkAndSweep(defaultArg threshold 0xFFFu)) :> IGarbageCollector

[<Sealed>]
type ValueStack (capacity: int32) =
    let capacity = nativeint capacity
    do if capacity <= 0n then raise(ArgumentOutOfRangeException(nameof capacity, capacity, "The capacity of the stack must be positive"))
    let start = Marshal.AllocHGlobal capacity
    let previous = Stack<nativeint>()
    let mutable disposed = false
    let mutable remaining = capacity

    let allocated = Event<_>()
    let freed = Event<_>()

    member _.TryAllocate(size, address: outref<_>) =
        let size = nativeint size
        if size < 0n then raise(ArgumentOutOfRangeException(nameof size, size, "The size to allocate cannot be negative"))
        if remaining >= size then
            let addr = NativePtr.ofNativeInt<byte>(start + capacity - remaining)
            address <- NativePtr.toVoidPtr addr
            allocated.Trigger(struct {| Size = size; Address = NativePtr.toNativeInt addr |})
            remaining <- remaining - size
            true
        else false

    member _.SaveAllocations() =
        previous.Push remaining

    member _.FreeAllocations() =
        let current = remaining
        remaining <- previous.Pop()
        freed.Trigger(remaining - current)

    [<CLIEvent>] member _.Allocated = allocated.Publish
    [<CLIEvent>] member _.Freed = freed.Publish

    override stack.Finalize() = (stack :> IDisposable).Dispose()

    interface IDisposable with
        member stack.Dispose() =
            if not disposed then
                Marshal.FreeHGlobal start
                remaining <- 0n
                disposed <- true
            GC.SuppressFinalize stack
