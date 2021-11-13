/// Contains functions for generating performance profiles in speedscope format (https://github.com/jlfwong/speedscope).
[<RequireQualifiedAccess>]
module UByte.Interpreter.Speedscope

open System

open UByte.Runtime

[<Struct>]
type FrameEventKind = | OpenFrame | CloseFrame

[<NoComparison; NoEquality>]
type FrameEvent =
    { Time: TimeSpan
      Type: FrameEventKind
      Frame: Interpreter.StackFrame }

val writeToStream :
    output: System.IO.Stream ->
    events: System.Collections.Immutable.ImmutableArray<FrameEvent> ->
    startTime: TimeSpan ->
    endTime: TimeSpan ->
    programFileName: string ->
    unit

val write :
    output : System.Text.Json.Utf8JsonWriter ->
    events: System.Collections.Immutable.ImmutableArray<FrameEvent> ->
    startTime: TimeSpan ->
    endTime: TimeSpan ->
    programFileName: string ->
    unit
