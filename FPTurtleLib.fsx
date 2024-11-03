#load "Common.fsx"

open Common

module Turtle =
  type TurtleState =
    { position: Position
      angle: float<Degrees>
      color: PenColor
      penState: PenState }

  let initialTurtleState =
    { position = initialPosition
      angle = 0.0<Degrees>
      color = initialColor
      penState = initialPenState }

  // not that state is LAST param in all the functions

  let move log distance state =
    log $"Move %0.1f{distance}"
    let newPosition = calcNewPosition distance state.angle state.position

    if state.penState = Down then
      dummyDrawLine log state.position newPosition state.color

    { state with position = newPosition }

  let turn log angle state =
    log $"Turn %0.1f{angle}"
    let newAngle = (state.angle + angle) % 360.0<Degrees>
    { state with angle = newAngle }

  let penUp log state =
    log "Pen up"
    { state with penState = Up }

  let penDown log state =
    log "Pen down"
    { state with penState = Down }

  let setColor log color state =
    log $"Set color %A{color}"
    { state with color = color }
