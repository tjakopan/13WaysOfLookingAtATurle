#load "Common.fsx"
#load "FPTurtleLib.fsx"
#load "TurtleApiHelpers.fsx"

open Common
open FPTurtleLib
open TurtleApiHelpers

module TurtleApiLayer =
  open Result

  let log message = printfn $"%s{message}"

  let move = Turtle.move log
  let turn = Turtle.turn log
  let penDown = Turtle.penDown log
  let penUp = Turtle.penUp log
  let setColor = Turtle.setColor log

  type TurtleApi() =
    let mutable state = Turtle.initialTurtleState

    let updateState newState = state <- newState

    member this.Exec(commandStr: string) =
      let tokens = commandStr.Split(' ') |> List.ofArray |> List.map trimString

      let stateR = returnR state

      let newStateR =
        match tokens with
        | [ "Move"; distanceStr ] ->
          let distanceR = validateDistance distanceStr
          lift2R move distanceR stateR
        | [ "Turn"; angleStr ] ->
          let angleR = validateAngle angleStr
          lift2R turn angleR stateR
        | [ "Pen"; "Up" ] -> returnR (penUp state)
        | [ "Pen"; "Down" ] -> returnR (penDown state)
        | [ "SetColor"; colorStr ] ->
          let colorR = validateColor colorStr
          lift2R setColor colorR stateR
        | _ -> Error(InvalidCommand commandStr)

      Result.map updateState newStateR

module TurtleApiClient =
  open TurtleApiLayer
  open Result

  let drawTriangle () =
    let api = TurtleApi()

    result {
      do! api.Exec "Move 100"
      do! api.Exec "Turn 120"
      do! api.Exec "Move 100"
      do! api.Exec "Turn 120"
      do! api.Exec "Move 100"
      do! api.Exec "Turn 120"
    }

  let drawThreeLines () =
    let api = TurtleApi()

    result {
      do! api.Exec "Pen Down"
      do! api.Exec "SetColor Black"
      do! api.Exec "Move 100"

      do! api.Exec "Pen Up"
      do! api.Exec "Turn 90"
      do! api.Exec "Move 100"
      do! api.Exec "Turn 90"

      do! api.Exec "Pen Down"
      do! api.Exec "SetColor Red"
      do! api.Exec "Move 100"

      do! api.Exec "Pen Up"
      do! api.Exec "Turn 90"
      do! api.Exec "Move 100"
      do! api.Exec "Turn 90"

      do! api.Exec "Pen Down"
      do! api.Exec "SetColor Blue"
      do! api.Exec "Turn 45"
      do! api.Exec "Move 100"
    }

  let drawPolygon n =
    let angle = 180.0 - (360.0 / float n)
    let api = TurtleApi()

    let drawOneSide () =
      result {
        do! api.Exec "Move 100"
        do! api.Exec $"Turn %f{angle}"
      }

    result {
      for i in [ 1..n ] do
        do! drawOneSide ()
    }

  let triggerError () =
    let api = TurtleApi()
    api.Exec "InvalidCommand 100"

TurtleApiClient.drawTriangle ()
TurtleApiClient.drawThreeLines ()
TurtleApiClient.drawPolygon 4
