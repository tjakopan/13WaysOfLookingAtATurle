#load "Common.fsx"
#load "FPTurtleLib.fsx"
#load "TurtleApiHelpers.fsx"

open Common
open FPTurtleLib
open TurtleApiHelpers

module TurtleApi_PassInSingleFunction =

  open Result

  type TurtleCommand =
    | Move of Distance
    | Turn of Angle
    | PenUp
    | PenDown
    | SetColor of PenColor

  type TurtleApi() =

    let mutable state = Turtle.initialTurtleState

    let updateState newState = state <- newState

    member this.Exec turtleFn (commandStr: string) =
      let tokens = commandStr.Split(' ') |> List.ofArray |> List.map trimString

      match tokens with
      | [ "Move"; distanceStr ] ->
        result {
          let! distance = validateDistance distanceStr
          let command = Move distance
          let newState = turtleFn command state
          updateState newState
        }
      | [ "Turn"; angleStr ] ->
        result {
          let! angle = validateAngle angleStr
          let command = Turn angle
          let newState = turtleFn command state
          updateState newState
        }
      | [ "Pen"; "Up" ] ->
        result {
          let command = PenUp
          let newState = turtleFn command state
          updateState newState
        }
      | [ "Pen"; "Down" ] ->
        result {
          let command = PenDown
          let newState = turtleFn command state
          updateState newState
        }
      | [ "SetColor"; colorStr ] ->
        result {
          let! color = validateColor colorStr
          let command = SetColor color
          let newState = turtleFn command state
          updateState newState
        }
      | _ -> Error(InvalidCommand commandStr)

module TurtleImplementation_PassInSingleFunction =

  open TurtleApi_PassInSingleFunction

  let log = printfn "%s"
  let move = Turtle.move log
  let turn = Turtle.turn log
  let penUp = Turtle.penUp log
  let penDown = Turtle.penDown log
  let setColor = Turtle.setColor log

  let normalSize () =
    let turtleFn =
      function
      | Move dist -> move dist
      | Turn angle -> turn angle
      | PenUp -> penUp
      | PenDown -> penDown
      | SetColor color -> setColor color

    let api = TurtleApi()
    api.Exec turtleFn

  let halfSize () =
    let turtleFn =
      function
      | Move dist -> move (dist / 2.0)
      | Turn angle -> turn angle
      | PenUp -> penUp
      | PenDown -> penDown
      | SetColor color -> setColor color

    let api = TurtleApi()
    api.Exec turtleFn

module TurtleApiClient_PassInSingleFunction =

  open Result

  type ApiFunction = string -> Result<unit, ErrorMessage>

  let drawTriangle (api: ApiFunction) =
    result {
      do! api "Move 100"
      do! api "Turn 120"
      do! api "Move 100"
      do! api "Turn 120"
      do! api "Move 100"
      do! api "Turn 120"
    }
    |> ignore

do
  let api = TurtleImplementation_PassInSingleFunction.normalSize ()
  TurtleApiClient_PassInSingleFunction.drawTriangle (api)

do
  let api = TurtleImplementation_PassInSingleFunction.halfSize ()
  TurtleApiClient_PassInSingleFunction.drawTriangle (api)
