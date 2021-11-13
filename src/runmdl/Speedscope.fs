﻿[<RequireQualifiedAccess>]
module UByte.Interpreter.Speedscope

open System
open System.Collections.Immutable
open System.Text.Json

module Runtime = UByte.Runtime.Interpreter

[<Struct>]
type FrameEventKind = | OpenFrame | CloseFrame

type FrameEvent = { Time: TimeSpan; Type: FrameEventKind; Frame: Runtime.StackFrame }

let write
    (output: Utf8JsonWriter)
    (events: ImmutableArray<FrameEvent>)
    (startTime: TimeSpan)
    (endTime: TimeSpan)
    (programFileName: string)
    =
    try
        output.WriteStartObject()
        output.WriteString("$schema", "https://www.speedscope.app/file-format-schema.json")

        output.WriteStartObject "shared"
        output.WriteStartArray "frames"

        let frames = System.Collections.Generic.Dictionary<Runtime.StackFrame, int32>()
        for { Frame = frame } in events do
            match frames.TryGetValue frame with
            | true, _ -> ()
            | false, _ ->
                let i = frames.Count
                frames.Add(frame, i)
                output.WriteStartObject()
                output.WriteString("name", frame.CurrentMethod.ToString())
                //output.WriteString("file", frame.CurrentModule.ToString())
                output.WriteEndObject()

        output.WriteEndArray() // frames
        output.WriteEndObject() // shared

        output.WriteStartArray "profiles"
        output.WriteStartObject()
        output.WriteString("type", "evented")
        output.WriteString("name", programFileName)
        output.WriteString("unit", "milliseconds")
        output.WriteNumber("startValue", startTime.Milliseconds)
        output.WriteNumber("endValue", endTime.Milliseconds)
        output.WriteStartArray "events"

        for event in events do
            let kind =
                match event.Type with
                | OpenFrame -> "O"
                | CloseFrame -> "C"

            output.WriteStartObject()
            output.WriteString("type", kind)
            output.WriteNumber("at", event.Time.Milliseconds)
            output.WriteNumber("frame", frames.[event.Frame])
            output.WriteEndObject()

        output.WriteEndArray() // events
        output.WriteEndObject()
        output.WriteEndArray() // profiles

        output.WriteEndObject()
        output.Flush()
    finally
        if output <> null then output.Dispose()

let writeToStream (output: System.IO.Stream) events startTime endTime programFileName =
    write (new Utf8JsonWriter(output)) events startTime endTime programFileName