module UByte.Runtime.MemoryManagement

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type ObjectType = ObjectType of uint32

/// Represents a pointer to an object managed by a garbage collector, and may or may not be moved.
[<RequireQualifiedAccess; IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type ObjectReference =
    val Address : nativeint

    member IsNull : bool

    static member Null : ObjectReference

    override ToString : unit -> string

    interface IEquatable<ObjectReference>

val inline (|ObjectReference|) : o: ObjectReference -> nativeint

[<RequireQualifiedAccess>]
module ObjectReference =
    val inline toNativePtr<'T when 'T : unmanaged> : o: ObjectReference -> nativeptr<'T>
    val inline toVoidPtr : o: ObjectReference -> voidptr
    val inline toNativeInt : o: ObjectReference -> nativeint

[<RequireQualifiedAccess; IsReadOnly; Struct; NoComparison; NoEquality>]
type ObjectRelocation = { From: ObjectReference; To: ObjectReference }

[<Interface>]
type IGarbageCollector =
    inherit IDisposable

    /// <summary>Allocates an object of the specified size, without zeroing out memory.</summary>
    /// <param name="state">Describes the objects that are currently considered roots.</param>
    /// <param name="size">The size, in bytes, of the object to allocate.</param>
    /// <exception cref="T:System.ArgumentOutOfRangeException">Thrown when the <paramref name="size"/> is negative.</exception>
    abstract Allocate : state: IGarbageCollectionState<'RootEnumerator> * ObjectType * size: int32 -> ObjectReference

    /// Frees all objects that are not referenced by any roots.
    abstract Collect : state: IGarbageCollectionState<'RootEnumerator> -> unit

    /// Returns a value describing the type of an object.
    abstract TypeOf : ObjectReference -> ObjectType

    [<CLIEvent>]
    abstract Allocated : IEvent<struct(int32 * ObjectReference)>

    [<CLIEvent>]
    abstract CollectionStarted : IEvent<unit>

    [<CLIEvent>]
    abstract Collected : IEvent<nativeint>

    [<CLIEvent>]
    abstract CollectionEnded : IEvent<uint32>

    [<CLIEvent>]
    abstract Moved : IEvent<struct(nativeint * ObjectReference)>

and [<Interface>] IGarbageCollectionState<'RootEnumerator when 'RootEnumerator :> IEnumerator<ObjectReference>> =
    /// Returns an enumeration of all objects that are currently in use.
    abstract EnumerateRoots : unit -> 'RootEnumerator

    abstract GetTypeSize : ObjectType -> int32

    abstract GetReferencedObjects : IGarbageCollector * ObjectReference -> ImmutableArray<ObjectReference>

    abstract AdjustMovedObjects : relocations: byref<#IEnumerator<ObjectRelocation>> -> unit

[<AbstractClass; Sealed>]
type GarbageCollectors =
    /// A simple stop-the-world naive mark and sweep garbage collector.
    static member MarkAndSweep : unit -> IGarbageCollector

    /// A simple stop-the-world mark and compact garbage collector.
    static member MarkAndCompact : ?capacity: uint32 -> IGarbageCollector

[<Sealed>]
type ValueStack =
    /// <summary>Constructs a new stack to store values with the specified size.</summary>
    /// <param name="capacity">The maximum capacity of the stack, in bytes.</param>
    internal new : capacity: int32 -> ValueStack

    member internal TryAllocate : size: int32 * address: outref<voidptr> -> bool
    member internal SaveAllocations: unit -> unit
    member internal FreeAllocations: unit -> unit

    [<CLIEvent>] member Allocated : IEvent<struct {| Size: nativeint; Address: nativeint |}>
    [<CLIEvent>] member Freed : IEvent<nativeint>

    override Finalize : unit -> unit

    interface IDisposable

[<RequireQualifiedAccess; IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type StackPointer<'T when 'T : unmanaged> =
    val Address : nativeint

    override ToString : unit -> string

    interface IEquatable<StackPointer<'T>>

/// Represents a pointer to a value on the stack.
type stackptr<'T when 'T : unmanaged> = StackPointer<'T>

val inline (|StackPointer|) : address: stackptr<'T> -> nativeint when 'T : unmanaged

[<RequireQualifiedAccess>]
module StackPtr =
    val inline toNativePtr<'T when 'T : unmanaged> : address: stackptr<'T> -> nativeptr<'T>
    val inline toNativeInt<'T when 'T : unmanaged> : address: stackptr<'T> -> nativeint
    val inline toVoidPtr<'T when 'T : unmanaged> : address: stackptr<'T> -> voidptr

    val internal ofNativePtr<'T when 'T : unmanaged> : address: nativeptr<'T> -> stackptr<'T>
    val internal ofVoidPtr<'T when 'T : unmanaged> : address: voidptr -> stackptr<'T>
    val internal ofNativeInt<'T when 'T : unmanaged> : address: nativeint -> stackptr<'T>

    val inline internal read<'T when 'T : unmanaged> : address: stackptr<'T> -> 'T
    val inline internal write<'T when 'T : unmanaged> : address: stackptr<'T> -> value: 'T -> unit

    val internal add<'T when 'T : unmanaged> : address: stackptr<'T> -> int32 -> stackptr<'T>

    val inline internal reinterpret<'From, 'To when 'From : unmanaged and 'To : unmanaged> :
        address: stackptr<'From> -> stackptr<'To>
