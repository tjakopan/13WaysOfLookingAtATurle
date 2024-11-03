#load "Common.fsx"
#load "FPTurtleLib.fsx"

open Common
open Microsoft.FSharp.Collections

module TurtleCommandHandler =
  open FPTurtleLib

  let log message = printfn $"%s{message}"

  let move = Turtle.move log
  let turn = Turtle.turn log
  let penUp = Turtle.penUp log
  let penDown = Turtle.penDown log
  let setColor = Turtle.setColor log

  type TurtleCommand =
    | Move of Distance
    | Turn of Angle
    | PenUp
    | PenDown
    | SetColor of PenColor

  let applyCommand state command =
    match command with
    | Move distance -> move distance state
    | Turn angle -> turn angle state
    | PenUp -> penUp state
    | PenDown -> penDown state
    | SetColor color -> setColor color state

  let run aListOfCommands =
    aListOfCommands |> List.fold applyCommand Turtle.initialTurtleState

module TurtleCommandClient =
  open TurtleCommandHandler

  let drawTriangle () =
    let commands =
      [ Move 100.0
        Turn 120.0<Degrees>
        Move 100.0
        Turn 120.0<Degrees>
        Move 100.0
        Turn 120.0<Degrees> ]

    run commands

  let drawThreeLines () =
    let commands =
      [ PenDown
        SetColor Black
        Move 100.0

        PenUp
        Turn 90.0<Degrees>
        Move 100.0
        Turn 90.0<Degrees>

        PenDown
        SetColor Red
        Move 100.0

        PenUp
        Turn 90.0<Degrees>
        Move 100.0
        Turn 90.0<Degrees>

        PenDown
        SetColor Blue
        Turn 45.0<Degrees>
        Move 100.0 ]

    run commands

  let drawPolygon n =
    let angle = 180.0 - (360.0 / float n)
    let angleDegrees = angle * 1.0<Degrees>

    let drawOneSide sideNumber = [ Move 100.0; Turn angleDegrees ]

    let commands = [ 1..n ] |> List.collect drawOneSide

    run commands

TurtleCommandClient.drawTriangle ()
TurtleCommandClient.drawThreeLines ()
TurtleCommandClient.drawPolygon 4
