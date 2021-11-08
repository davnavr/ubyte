module UByte.Runtime.MemoryManagement

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
    val inline toBytePtr : o: ObjectReference -> nativeptr<uint8>
    val inline toVoidPtr : o: ObjectReference -> voidptr

[<Interface>]
type IGarbageCollector =
    /// <summary>Allocates an object of the specified size, without zeroing out memory.</summary>
    /// <param name="size">The size, in bytes, of the object to allocate.</param>
    /// <exception cref="T:System.ArgumentOutOfRangeException">Thrown when the <paramref name="size"/> is negative.</exception>
    abstract Allocate : ObjectType * size: int32 -> ObjectReference
    /// Frees all objects that are not referenced by any roots.
    abstract Collect : (IGarbageCollector -> ObjectReference -> ImmutableArray<ObjectReference>) -> unit
    /// Returns a value describing the type of an object.
    abstract TypeOf : ObjectReference -> ObjectType
    /// The objects that are always reachable, such as objects in local variables, arguments, global variables, etc.
    abstract Roots : System.Collections.Generic.ICollection<ObjectReference>

[<AbstractClass; Sealed>]
type CollectionStrategies =
    static member NaiveMarkAndSweep : unit -> IGarbageCollector
    static member NaiveMarkAndSweep : threshold: uint32 -> IGarbageCollector
