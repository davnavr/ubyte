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

let private getAddressString (address: nativeint) = sprintf "0x%08X" address

[<Struct>]
type ObjectReference =
    val Address : nativeint

    new (address) = { Address = address }

    member o.IsNull = o.Address = 0n

    static member Null = Unchecked.defaultof<ObjectReference>

    override o.ToString() = getAddressString o.Address

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

[<RequireQualifiedAccess; Struct>]
type ObjectRelocation = { From: ObjectReference; To: ObjectReference }

type IGarbageCollector =
    inherit IDisposable

    abstract Allocate : state: IGarbageCollectionState<'RootEnumerator> * ObjectType * size: int32 -> ObjectReference
    abstract Collect : state: IGarbageCollectionState<'RootEnumerator> -> unit
    abstract TypeOf : ObjectReference -> ObjectType
    [<CLIEvent>] abstract Allocated : IEvent<struct(int32 * ObjectReference)>
    [<CLIEvent>] abstract Collected : IEvent<nativeint>
    [<CLIEvent>] abstract Moved : IEvent<struct(nativeint * ObjectReference)>

and IGarbageCollectionState<'RootEnumerator when 'RootEnumerator :> IEnumerator<ObjectReference>> =
    abstract EnumerateRoots : unit -> 'RootEnumerator
    abstract GetTypeSize : ObjectType -> int32
    abstract GetReferencedObjects : IGarbageCollector * ObjectReference -> ImmutableArray<ObjectReference>
    abstract AdjustMovedObjects : relocations: byref<#IEnumerator<ObjectRelocation>> -> unit

[<AbstractClass>]
type ValidatedCollector () =
    let mutable disposed = false
    let allocated = Event<_>()

    member val Collected = Event<_>()
    member val Moved = Event<_>()

    /// Frees all objects allocated by this collector.
    abstract Deallocate : finalizing: bool -> unit

    abstract Allocate : state: IGarbageCollectionState<'RootEnumerator> * ObjectType * size: int32 -> ObjectReference

    abstract Collect : state: IGarbageCollectionState<'RootEnumerator> -> unit

    abstract TypeOf : ObjectReference -> ObjectType

    member private gc.Dispose finalizing =
        if not disposed then
            gc.Deallocate finalizing
            disposed <- true

    member private gc.CheckDisposed() = if disposed then raise(ObjectDisposedException(nameof gc))

    interface IGarbageCollector with
        member gc.Dispose() =
            gc.Dispose false
            GC.SuppressFinalize gc

        member gc.Allocate(state, ty, size) =
            gc.CheckDisposed()
            if size < 0 then
                raise(ArgumentOutOfRangeException(nameof size, size, "Cannot allocate an object with a negative size"))
            let o = gc.Allocate(state, ty, size)
            allocated.Trigger(struct(size, o))
            o

        member gc.Collect state =
            gc.CheckDisposed()
            gc.Collect state

        member gc.TypeOf o =
            gc.CheckDisposed()
            gc.TypeOf o

        [<CLIEvent>] member _.Allocated = allocated.Publish
        [<CLIEvent>] member gc.Collected = gc.Collected.Publish
        [<CLIEvent>] member gc.Moved = gc.Moved.Publish

    /// DO NOT OVERRIDE THIS
    override gc.Finalize() = gc.Dispose true

/// Ensures that a lock on an object is acquired when an allocation or collection is made.
[<Sealed>]
type PausingCollector<'Collector when 'Collector :> IGarbageCollector> (collector: 'Collector) =
    let lobj = obj()

    interface IGarbageCollector with
        member _.Dispose() = collector.Dispose()
        member _.Allocate(state, ty, size) = lock lobj (fun() -> collector.Allocate(state, ty, size))
        member _.Collect state = lock lobj (fun() -> collector.Collect state)
        member _.TypeOf o = collector.TypeOf o
        [<CLIEvent>] member _.Allocated = collector.Allocated
        [<CLIEvent>] member _.Collected = collector.Collected
        [<CLIEvent>] member _.Moved = collector.Moved

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
    | Pinned = 2u

[<RequireQualifiedAccess; Struct; NoComparison; NoEquality>]
type MarkAndSweepHeader =
    { mutable Flags: MarkAndSweepFlags
      Type: ObjectType
      mutable Next: ObjectReference }

let markAndSweepNext o =
    let header = &ObjectReference.getHeaderRef<MarkAndSweepHeader> o
    &header.Next

let markAndSweepFlags o =
   let header = &ObjectReference.getHeaderRef<MarkAndSweepHeader> o
   &header.Flags

// TODO: Make an interface IMarkableHeader to access flags faster/more easily?
let markUsedObjects (state: IGarbageCollectionState<_>) gc (unmarked: Stack<_>) getHeaderFlags markObjectReference isNotMarked =
    unmarked.Clear()
    let mutable objectRootEnumerator = state.EnumerateRoots()
    while objectRootEnumerator.MoveNext() do
        let root = objectRootEnumerator.Current
        if not root.IsNull then unmarked.Push root

    while unmarked.Count > 0 do
        let o = unmarked.Pop()
        markObjectReference o
        for referenced in state.GetReferencedObjects(gc, o) do
            if not referenced.IsNull && isNotMarked(getHeaderFlags referenced) then
                unmarked.Push referenced

[<Sealed>]
type MarkAndSweep () =
    inherit ValidatedCollector()

    let mutable first = ObjectReference.Null
    let mutable last = ObjectReference.Null
    let mutable allocated = 0u
    /// If the number of allocated bytes exceeds this number, a collection occurs.
    let mutable threshold = 0u
    let unmarked = Stack()

    override gc.Allocate(state, ty, size) =
        let o = alloch<MarkAndSweepHeader> size

        let header = &ObjectReference.getHeaderRef<MarkAndSweepHeader> o
        header.Flags <- MarkAndSweepFlags.Pinned
        Unsafe.AsRef &header.Type <- ty
        header.Next <- ObjectReference.Null

        if allocated > threshold then
            gc.Collect state
            threshold <- allocated

        if first.IsNull then first <- o
        if not last.IsNull then markAndSweepNext last <- o
        last <- o
        o

    override _.TypeOf o =
        let header : inref<_> = &ObjectReference.getHeaderRef<MarkAndSweepHeader> o
        header.Type

    member private gc.Free(finalizing, o) =
        freeh<MarkAndSweepHeader> o
        if not finalizing then
            gc.Collected.Trigger(ObjectReference.toNativeInt o)

    override gc.Collect state =
        let inline isNotMarked flags = flags &&& MarkAndSweepFlags.Marked = MarkAndSweepFlags.None

        // Mark
        markUsedObjects
            state
            gc
            unmarked
            (fun o -> markAndSweepFlags o) 
            (fun o ->
                let oflags = &markAndSweepFlags o
                oflags <- oflags ||| MarkAndSweepFlags.Marked)
            isNotMarked

        // Sweep
        let mutable current, previous = first, ObjectReference.Null
        while not current.IsNull do
            let header = &ObjectReference.getHeaderRef<MarkAndSweepHeader> current
            let next = header.Next

            if isNotMarked header.Flags then
                if current = first then first <- header.Next
                if current = last then last <- previous
                gc.Free(false, current)
                if not previous.IsNull then markAndSweepNext previous <- next
            else
                header.Flags <- header.Flags &&& (~~~MarkAndSweepFlags.Marked)
                previous <- current

            current <- next

    override gc.Deallocate finalizing =
        let mutable current = first
        while not current.IsNull do
            let next = markAndSweepNext current
            gc.Free(finalizing, current)
            current <- next

[<RequireQualifiedAccess; Struct; NoComparison; NoEquality>]
type MarkAndCompactHeader =
    { mutable Flags: MarkAndSweepFlags
      Type: ObjectType }

[<RequireQualifiedAccess; IsReadOnly; Struct; NoComparison; NoEquality>]
type FreeBlock = { Offset: nativeint; Size: nativeint }

[<Sealed>]
type MarkAndCompact (capacity: uint32) =
    inherit ValidatedCollector()

    let capacity = nativeint capacity
    let start = Marshal.AllocHGlobal capacity
    let mutable remaining = capacity
    /// If the number of remaining bytes is below this number, a garbage collection and compaction occurs.
    let mutable threshold = IntPtr.MaxValue
    let free = Queue<FreeBlock>()
    let unmarked = Stack()
    let relocations = List<ObjectRelocation>()

    member private _.NextAddress() = start + capacity - remaining

    member private gc.NextObject() =
        ObjectReference(nativeint sizeof<MarkAndCompactHeader> + gc.NextAddress())

    override gc.Allocate(state, ty, size) =
        let osize = nativeint sizeof<MarkAndCompactHeader> + nativeint size
        if osize > remaining then
            ObjectReference.Null
        else
            if free.Count > 0 then
                raise(NotImplementedException "TODO: Search free list for blocks")
            let o = gc.NextObject()

            remaining <- remaining - osize
            if remaining < threshold then
                gc.Collect state
                if remaining < threshold then threshold <- remaining

            let header = &ObjectReference.getHeaderRef<MarkAndCompactHeader> o
            header.Flags <- MarkAndSweepFlags.None
            Unsafe.AsRef &header.Type <- ty

            o

    override gc.Collect state =
        if remaining < capacity then
            // Mark
            markUsedObjects
                state
                gc
                unmarked
                (fun o ->
                    let header = &ObjectReference.getHeaderRef<MarkAndCompactHeader> o
                    header.Flags)
                (fun o ->
                    let header = &ObjectReference.getHeaderRef<MarkAndCompactHeader> o
                    header.Flags <- header.Flags ||| MarkAndSweepFlags.Marked)
                (fun flags -> flags &&& MarkAndSweepFlags.Marked = MarkAndSweepFlags.None)

            // Compact
            relocations.Clear()

            /// Pointer to the first byte of the object header.
            let mutable current = start
            /// Pointer where the next object will be copied to.
            let mutable destination = start
            let mutable remainingFreeBlocks = free.Count
            let heapEndAddress = gc.NextAddress()

            while current < heapEndAddress do
#if DEBUG
                let skippedFreeBlock() =
#else
                let inline skippedFreeBlock() =
#endif
                    let mutable nextFreeBlock = Unchecked.defaultof<_>
                    if free.TryPeek &nextFreeBlock then
                        let nextObjectAddress = nextFreeBlock.Offset + nextFreeBlock.Size
                        // Checks if we are currently in a free block.
                        if remainingFreeBlocks > 0 && nextFreeBlock.Offset >= current && current < nextObjectAddress then
                            remainingFreeBlocks <- remainingFreeBlocks - 1
                            // Skips to the address of the next object.
                            current <- nextObjectAddress
                            // Destination address does not need to be updated here.
                            true
                        else
                            false
                    else
                        false

                if not(skippedFreeBlock()) then
                    let hsize = nativeint sizeof<MarkAndCompactHeader>
                    let oaddr = current + hsize
                    let o = ObjectReference oaddr
                    let mutable header = &ObjectReference.getHeaderRef<MarkAndCompactHeader> o
                    let osize = hsize + nativeint(state.GetTypeSize header.Type)
                    let next = current + osize

                    header.Flags <- header.Flags &&& (~~~MarkAndSweepFlags.Marked)

                    if header.Flags &&& MarkAndSweepFlags.Marked = MarkAndSweepFlags.None then
                        // Dead objects are simply overwritten when used objects are compacted.
                        destination <- current
                        gc.Collected.Trigger oaddr
                    elif destination < current then
                        if header.Flags &&& MarkAndSweepFlags.Pinned = MarkAndSweepFlags.None then
                            let oadjusted = ObjectReference(destination + hsize)
                            Unsafe.CopyBlock(ObjectReference.toVoidPtr oadjusted, ObjectReference.toVoidPtr o, uint32 osize)
                            relocations.Add { ObjectRelocation.From = o; ObjectRelocation.To = oadjusted }
                            destination <- destination + osize
                            gc.Moved.Trigger(struct(ObjectReference.toNativeInt o, oadjusted))
                        else
                            free.Enqueue { FreeBlock.Offset = destination; FreeBlock.Size = current - destination }
                            destination <- next

                    current <- next

            remaining <- current - start

            // Adjust
            let mutable relocatedObjectsEnumerator = relocations.GetEnumerator()
            state.AdjustMovedObjects &relocatedObjectsEnumerator

    override _.TypeOf o =
        let header : inref<_> = &ObjectReference.getHeaderRef<MarkAndCompactHeader> o
        header.Type

    override _.Deallocate finalizing =
        Marshal.FreeHGlobal start
        remaining <- 0n
        threshold <- 0n
        if not finalizing then free.Clear()

[<AbstractClass; Sealed>]
type GarbageCollectors =
    static member MarkAndSweep() =
        new PausingCollector<_>(new MarkAndSweep()) :> IGarbageCollector

    static member MarkAndCompact(?capacity) =
        new PausingCollector<_>(new MarkAndCompact(defaultArg capacity 0xFFFFFu)) :> IGarbageCollector

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
            let addr = start + capacity - remaining
            assert(start + addr < start + capacity)
            address <- NativePtr.toVoidPtr(NativePtr.ofNativeInt<byte> addr)
            allocated.Trigger(struct {| Size = size; Address = addr |})
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

[<Struct; StructuralComparison; StructuralEquality>]
type StackPointer<'T when 'T : unmanaged> =
    val Address : nativeint

    new (address) = { Address = address }

    override ptr.ToString() = getAddressString ptr.Address

type stackptr<'T when 'T : unmanaged> = StackPointer<'T>

let inline (|StackPointer|) (address: stackptr<'T>) = address.Address

[<RequireQualifiedAccess>]
module StackPtr =
    let inline toNativePtr (StackPointer address: stackptr<'T>) = NativePtr.ofNativeInt<'T> address
    let inline toNativeInt (StackPointer address: stackptr<'T>) = address
    let inline toVoidPtr (address: stackptr<'T>) = NativePtr.toVoidPtr<'T>(toNativePtr address)

    let ofNativeInt<'T when 'T : unmanaged> address = stackptr<'T> address
    let ofNativePtr<'T when 'T : unmanaged> address = ofNativeInt<'T>(NativePtr.toNativeInt<'T> address)
    let ofVoidPtr address = ofNativePtr<'T>(NativePtr.ofVoidPtr<'T> address)

    let inline read (address: stackptr<'T>) = NativePtr.read(toNativePtr address)
    let inline write (address: stackptr<'T>) (value: 'T) = NativePtr.write (toNativePtr address) value

    let add<'T when 'T : unmanaged> (StackPointer address: stackptr<'T>) index =
        ofNativeInt<'T>(address + (nativeint index * nativeint sizeof<'T>))

    let inline reinterpret<'From, 'To when 'From : unmanaged and 'To : unmanaged> (StackPointer address: stackptr<'From>) =
        ofNativeInt<'To> address
