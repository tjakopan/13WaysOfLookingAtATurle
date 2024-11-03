#load "Common.fsx"
#load "OOTurtleLib.fsx"


open Common
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core
open OOTurtleLib

module TurtleApiLayer =

  exception TurtleApiException of string

  let log message = printfn $"%s{message}"

  type TurtleApi() =
    let turtle = Turtle(log)

    let validateDistance distanceStr =
      try
        float distanceStr
      with ex ->
        let msg = $"Invalid distance '%s{distanceStr}': %s{ex.Message}"
        raise (TurtleApiException msg)

    let validateAngle angleStr =
      try
        (float angleStr) * 1.0<Degrees>
      with ex ->
        let msg = $"Invalid angle '%s{angleStr}': %s{ex.Message}"
        raise (TurtleApiException msg)

    let validateColor colorStr =
      match colorStr with
      | "Black" -> Black
      | "Red" -> Red
      | "Blue" -> Blue
      | _ ->
        let msg = $"Invalid color '%s{colorStr}'"
        raise (TurtleApiException msg)

    member this.Exec(commandStr: string) =
      let tokens = commandStr.Split(' ') |> List.ofArray |> List.map trimString

      match tokens with
      | [ "Move"; distanceStr ] ->
        let distance = validateDistance distanceStr
        turtle.Move distance
      | [ "Turn"; angleStr ] ->
        let angle = validateAngle angleStr
        turtle.Turn angle
      | [ "Pen"; "Up" ] -> turtle.PenUp()
      | [ "Pen"; "Down" ] -> turtle.PenDown()
      | [ "SetColor"; colorStr ] ->
        let color = validateColor colorStr
        turtle.SetColor color
      | _ ->
        let msg = $"Invalid command '%s{commandStr}'"
        raise (TurtleApiException msg)

module TurtleApiClient =
  open TurtleApiLayer

  let drawTriangle () =
    let api = TurtleApi()
    api.Exec "Move 100"
    api.Exec "Turn 120"
    api.Exec "Move 100"
    api.Exec "Turn 120"
    api.Exec "Move 100"
    api.Exec "Turn 120"

  let drawThreeLines () =
    let api = TurtleApi()
    // draw black line
    api.Exec "Pen Down"
    api.Exec "SetColor Black"
    api.Exec "Move 100"
    // move without drawing
    api.Exec "Pen Up"
    api.Exec "Turn 90"
    api.Exec "Move 100"
    api.Exec "Turn 90"
    // draw red line
    api.Exec "Pen Down"
    api.Exec "SetColor Red"
    api.Exec "Move 100"
    // move without drawing
    api.Exec "Pen Up"
    api.Exec "Turn 90"
    api.Exec "Move 100"
    api.Exec "Turn 90"
    // back home at (0,0) with angle 0
    // draw diagonal blue line
    api.Exec "Pen Down"
    api.Exec "SetColor Blue"
    api.Exec "Turn 45"
    api.Exec "Move 100"

  let drawPolygon n =
    let angle = 180.0 - (360.0 / float n)
    let api = TurtleApi()

    let drawOneSide () =
      api.Exec "Move 100"
      api.Exec $"Turn %f{angle}"

    for i in [ 1..n ] do
      drawOneSide ()

  let triggerError () =
    let api = TurtleApi()
    api.Exec "InvalidCommand 100"

TurtleApiClient.drawTriangle ()
TurtleApiClient.drawThreeLines ()
TurtleApiClient.drawPolygon 4
TurtleApiClient.triggerError ()
