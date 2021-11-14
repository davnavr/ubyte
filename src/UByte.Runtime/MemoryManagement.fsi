﻿module UByte.Runtime.MemoryManagement

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type ObjectType = ObjectType of uint32

[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type ObjectReference =
    val Address : nativeint

    member IsNull : bool

    static member Null : ObjectReference

    override ToString : unit -> string

    interface System.IEquatable<ObjectReference>

val inline (|ObjectReference|) : o: ObjectReference -> nativeint

[<RequireQualifiedAccess>]
module ObjectReference =
    val inline toNativePtr<'T when 'T : unmanaged> : o: ObjectReference -> nativeptr<'T>
    val inline toVoidPtr : o: ObjectReference -> voidptr

//IGarbageCollectionStrategy
[<Interface>]
type IGarbageCollector =
    inherit IDisposable

    /// <summary>Allocates an object of the specified size, without zeroing out memory.</summary>
    /// <param name="size">The size, in bytes, of the object to allocate.</param>
    /// <exception cref="T:System.ArgumentOutOfRangeException">Thrown when the <paramref name="size"/> is negative.</exception>
    abstract Allocate : ObjectType * size: int32 -> ObjectReference
    /// Frees all objects that are not referenced by any roots.
    abstract Collect : (IGarbageCollector -> ObjectReference -> ImmutableArray<ObjectReference>) -> unit
    /// Returns a value describing the type of an object.
    abstract TypeOf : ObjectReference -> ObjectType
    /// The objects that are always reachable, such as objects in local variables, arguments, global variables, etc.
    abstract Roots : System.Collections.Generic.ICollection<ObjectReference> // TODO: Could be a IList, so same object can be added more than once, helps simplify rooting of objects stored in locals.

// TODO: Have NaiveMarkAndSweep class be public instead to force usage of 'new' keyword since it is disposable

[<AbstractClass; Sealed>]
type CollectionStrategies =
    static member NaiveMarkAndSweep : unit -> IGarbageCollector
    static member NaiveMarkAndSweep : threshold: uint32 -> IGarbageCollector

[<Sealed>]
type ValueStack =
    /// <summary>Constructs a new stack to store values with the specified size.</summary>
    /// <param name="capacity">The maximum capacity of the stack, in bytes.</param>
    internal new: capacity: int32 -> ValueStack

    member internal TryAllocate : size: int32 * address: outref<voidptr> -> bool
    member internal SaveAllocations: unit -> unit
    member internal FreeAllocations: unit -> unit

    [<CLIEvent>] member Allocated : IEvent<struct {| Size: nativeint; Address: nativeint |}>
    [<CLIEvent>] member Freed : IEvent<nativeint>

    override Finalize : unit -> unit

    interface IDisposable
