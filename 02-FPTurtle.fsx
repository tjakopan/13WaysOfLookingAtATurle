#load "Common.fsx"
#load "FPTurtleLib.fsx"

open Common
open FPTurtleLib

module FPTurtleClient =
  let log message = printfn $"%s{message}"

  let move = Turtle.move log
  let turn = Turtle.turn log
  let penDown = Turtle.penDown log
  let penUp = Turtle.penUp log
  let setColor = Turtle.setColor log

  let drawTriangle () =
    Turtle.initialTurtleState
    |> move 100.0
    |> turn 120.0<Degrees>
    |> move 100.0
    |> turn 120.0<Degrees>
    |> move 100.0
    |> turn 120.0<Degrees>
  // back home at (0,0) with angle 0

  let drawThreeLines () =
    Turtle.initialTurtleState
    |> penDown
    |> setColor Black
    |> move 100.0
    |> penUp
    |> turn 90.0<Degrees>
    |> move 100.0
    |> turn 90.0<Degrees>
    |> penDown
    |> setColor Red
    |> move 100.0
    |> penUp
    |> turn 90.0<Degrees>
    |> move 100.0
    |> turn 90.0<Degrees>
    |> penDown
    |> setColor Blue
    |> turn 45.0<Degrees>
    |> move 100.0

  let drawPolygon n =
    let angle = 180.0 - (360.0 / float n)
    let angleDegrees = angle * 1.0<Degrees>

    let oneSide state sideNumber =
      state |> move 100.0 |> turn angleDegrees

    [ 1..n ] |> List.fold oneSide Turtle.initialTurtleState

FPTurtleClient.drawTriangle ()
FPTurtleClient.drawThreeLines ()
FPTurtleClient.drawPolygon 4
