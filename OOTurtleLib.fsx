#load "Common.fsx"

open Common

type Turtle(log) =
  let mutable currentPosition = initialPosition
  let mutable currentAngle = 0.0<Degrees>
  let mutable currentColor = initialColor
  let mutable currentPenState = initialPenState

  member this.Move(distance) =
    log $"Move %0.1f{distance}"
    let newPosition = calcNewPosition distance currentAngle currentPosition

    if currentPenState = Down then
      dummyDrawLine log currentPosition newPosition currentColor

    currentPosition <- newPosition

  member this.Turn(angle) =
    log $"Turn %0.1f{angle}"
    let newAngle = (currentAngle + angle) % 360.0<Degrees>
    currentAngle <- newAngle

  member this.PenUp() =
    log "Pen up"
    currentPenState <- Up

  member this.PenDown() =
    log "Pen down"
    currentPenState <- Down

  member this.SetColor(color) =
    log $"Set color %A{color}"
    currentColor <- color
