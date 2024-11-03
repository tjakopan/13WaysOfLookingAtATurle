#load "Common.fsx"
#load "FPTurtleLib.fsx"

open Common
open FPTurtleLib
open Microsoft.FSharp.Collections

type TurtleStateComputation<'a> = TurtleStateComputation of (Turtle.TurtleState -> 'a * Turtle.TurtleState)

module TurtleStateComputation =
  let runT turtle state =
    let (TurtleStateComputation innerFn) = turtle
    innerFn state

  let returnT x =
    let innerFn state = (x, state)
    TurtleStateComputation innerFn

  let bindT f xT =
    let innerFn state =
      let x, state2 = runT xT state
      runT (f x) state2

    TurtleStateComputation innerFn

  let mapT f = bindT (f >> returnT)

  let toComputation f =
    let innerFn state =
      let result, newState = f state
      (result, newState)

    TurtleStateComputation innerFn

  let toUnitComputation f =
    let f2 state = (), f state
    toComputation f2

  type TurtleBuilder() =
    member this.Return(x) = returnT x
    member this.Bind(x, f) = bindT f x

  let turtle = TurtleBuilder()

module TurtleComputationClient =
  open TurtleStateComputation

  let log message = printfn $"%s{message}"

  let initialTurtleState = Turtle.initialTurtleState

  let move dist =
    toUnitComputation (Turtle.move log dist)

  let turn angle =
    toUnitComputation (Turtle.turn log angle)

  let penUp = toUnitComputation (Turtle.penUp log)

  let penDown = toUnitComputation (Turtle.penDown log)

  let setColor color =
    toUnitComputation (Turtle.setColor log color)

  let drawTriangle () =
    let t =
      turtle {
        do! move 100.0
        do! turn 120.0<Degrees>
        do! move 100.0
        do! turn 120.0<Degrees>
        do! move 100.0
        do! turn 120.0<Degrees>
      }

    runT t initialTurtleState

  let drawThreeLines () =
    let t =
      turtle {
        do! penDown
        do! setColor Black
        do! move 100.0

        do! penUp
        do! turn 90.0<Degrees>
        do! move 100.0
        do! turn 90.0<Degrees>

        do! penDown
        do! setColor Red
        do! move 100.0

        do! penUp
        do! turn 90.0<Degrees>
        do! move 100.0
        do! turn 90.0<Degrees>

        do! penDown
        do! setColor Blue
        do! turn 45.0<Degrees>
        do! move 100.0
      }

    runT t initialTurtleState

  let drawPolygon n =
    let angle = 180.0 - (360.0 / float n)
    let angleDegrees = angle * 1.0<Degrees>

    let oneSide =
      turtle {
        do! move 100.0
        do! turn angleDegrees
      }

    let chain f g =
      turtle {
        do! f
        do! g
      }

    let sides = List.replicate n oneSide

    let all = sides |> List.reduce chain

    runT all initialTurtleState

TurtleComputationClient.drawTriangle ()
TurtleComputationClient.drawThreeLines ()
TurtleComputationClient.drawPolygon 4
