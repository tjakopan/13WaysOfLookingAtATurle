#load "Common.fsx"
#load "OOTurtleLib.fsx"
#load "FPTurtleLib.fsx"

open Common

type TurtleState = FPTurtleLib.Turtle.TurtleState

type TurtleFunctions =
  { move: Distance -> TurtleState -> TurtleState
    turn: Angle -> TurtleState -> TurtleState
    penUp: TurtleState -> TurtleState
    penDown: TurtleState -> TurtleState
    setColor: PenColor -> TurtleState -> TurtleState }

module TurtleApiLayer_FP =
  open Result
  open FPTurtleLib

  let log message = printfn $"%s{message}"

  let initialTurtleState = Turtle.initialTurtleState

  type ErrorMessage =
    | InvalidDistance of string
    | InvalidAngle of string
    | InvalidColor of string
    | InvalidCommand of string

  let validateDistance distanceStr =
    try
      Ok(float distanceStr)
    with ex ->
      Error(InvalidDistance distanceStr)

  let validateAngle angleStr =
    try
      Ok((float angleStr) * 1.0<Degrees>)
    with ex ->
      Error(InvalidAngle angleStr)

  let validateColor colorStr =
    match colorStr with
    | "Black" -> Ok(Black)
    | "Red" -> Ok(Red)
    | "Blue" -> Ok(Blue)
    | _ -> Error(InvalidColor colorStr)

  type TurtleApi(turtleFunctions: TurtleFunctions) =
    let mutable state = initialTurtleState

    let updateState newState = state <- newState

    member this.Exec(commandStr: string) =
      let tokens = commandStr.Split(' ') |> List.ofArray |> List.map trimString

      match tokens with
      | [ "Move"; distanceStr ] ->
        result {
          let! distance = validateDistance distanceStr
          let newState = turtleFunctions.move distance state
          updateState newState
        }
      | [ "Turn"; angleStr ] ->
        result {
          let! angle = validateAngle angleStr
          let newState = turtleFunctions.turn angle state
          updateState newState
        }
      | [ "Pen"; "Up" ] ->
        result {
          let newState = turtleFunctions.penUp state
          updateState newState
        }
      | [ "Pen"; "Down" ] ->
        result {
          let newState = turtleFunctions.penDown state
          updateState newState
        }
      | [ "SetColor"; colorStr ] ->
        result {
          let! color = validateColor colorStr
          let newState = turtleFunctions.setColor color state
          updateState newState
        }
      | _ -> Error(InvalidCommand commandStr)

open TurtleApiLayer_FP

module TurtleImplementation_FP =
  open FPTurtleLib

  let normalSize () =
    let log = printfn "%s"

    { move = Turtle.move log
      turn = Turtle.turn log
      penUp = Turtle.penUp log
      penDown = Turtle.penDown log
      setColor = Turtle.setColor log }

  let halfSize () =
    let normalSize = normalSize ()

    { normalSize with
        move = fun dist -> normalSize.move (dist / 2.0) }

module TurtleApiClient_FP =
  open Result

  let drawTriangle (api: TurtleApi) =
    result {
      do! api.Exec "Move 100"
      do! api.Exec "Turn 120"
      do! api.Exec "Move 100"
      do! api.Exec "Turn 120"
      do! api.Exec "Move 100"
      do! api.Exec "Turn 120"
    }
    |> ignore

do
  let turtleFns = TurtleImplementation_FP.normalSize ()
  let api = TurtleApi(turtleFns)
  TurtleApiClient_FP.drawTriangle api

do
  let turtleFns = TurtleImplementation_FP.halfSize ()
  let api = TurtleApi(turtleFns)
  TurtleApiClient_FP.drawTriangle api
