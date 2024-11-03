#load "Common.fsx"
#load "FPTurtleLib.fsx"
#load "TurtleApiHelpers.fsx"

open Common
open FPTurtleLib
open TurtleApiHelpers

module TurtleApi_PassInAllFunctions =
  open Result

  type TurtleApi() =
    let mutable state = Turtle.initialTurtleState

    let updateState newState = state <- newState

    member this.Exec move turn penUp penDown setColor (commandStr: string) =
      let tokens = commandStr.Split(' ') |> List.ofArray |> List.map trimString

      match tokens with
      | [ "Move"; distanceStr ] ->
        result {
          let! distance = validateDistance distanceStr
          let newState = move distance state
          updateState newState
        }
      | [ "Turn"; angleStr ] ->
        result {
          let! angle = validateAngle angleStr
          let newState = turn angle state
          updateState newState
        }
      | [ "Pen"; "Up" ] ->
        result {
          let newState = penUp state
          updateState newState
        }
      | [ "Pen"; "Down" ] ->
        result {
          let newState = penDown state
          updateState newState
        }
      | [ "SetColor"; colorStr ] ->
        result {
          let! color = validateColor colorStr
          let newState = setColor color state
          updateState newState
        }
      | _ -> Error(InvalidCommand commandStr)


module TurtleImplementation_PassInAllFunctions =
  open TurtleApi_PassInAllFunctions

  let log = printfn "%s"
  let move = Turtle.move log
  let turn = Turtle.turn log
  let penUp = Turtle.penUp log
  let penDown = Turtle.penDown log
  let setColor = Turtle.setColor log

  let normalSize () =
    let api = TurtleApi()
    api.Exec move turn penUp penDown setColor

  let halfSize () =
    let moveHalf dist = move (dist / 2.0)
    let api = TurtleApi()
    api.Exec moveHalf turn penUp penDown setColor

module TurtleApiClient_PassInAllFunctions =

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
  let apiFn = TurtleImplementation_PassInAllFunctions.normalSize ()
  TurtleApiClient_PassInAllFunctions.drawTriangle apiFn

do
  let apiFn = TurtleImplementation_PassInAllFunctions.halfSize ()
  TurtleApiClient_PassInAllFunctions.drawTriangle apiFn

do
  let mockApi s =
    printfn $"[MockAPI] %s{s}"
    Ok()

  TurtleApiClient_PassInAllFunctions.drawTriangle mockApi
