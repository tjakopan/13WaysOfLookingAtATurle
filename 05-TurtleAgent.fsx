#load "Common.fsx"
#load "FPTurtleLib.fsx"
#load "TurtleApiHelpers.fsx"

open Common
open TurtleApiHelpers

module AgentImplementation =
  open FPTurtleLib

  type TurtleCommand =
    | Move of Distance
    | Turn of Angle
    | PenUp
    | PenDown
    | SetColor of PenColor

  type TurtleAgent() =
    let log message = printfn $"%s{message}"

    let move = Turtle.move log
    let turn = Turtle.turn log
    let penUp = Turtle.penUp log
    let penDown = Turtle.penDown log
    let setColor = Turtle.setColor log

    let mailboxProc =
      MailboxProcessor.Start(fun inbox ->
        let rec loop turtleState =
          async { 
            let! command = inbox.Receive()

            let newState =
              match command with
              | Move distance -> move distance turtleState
              | Turn angle -> turn angle turtleState
              | PenUp -> penUp turtleState
              | PenDown -> penDown turtleState
              | SetColor color -> setColor color turtleState

            return! loop newState
          }

        loop Turtle.initialTurtleState)

    member this.Post(command) = mailboxProc.Post command

module TurtleApiLayer =
  open Result
  open AgentImplementation

  type TurtleApi() =
    let turtleAgent = TurtleAgent()

    member this.Exec(commandStr: string) =
      let tokens = commandStr.Split(' ') |> List.ofArray |> List.map trimString

      let result =
        match tokens with
        | [ "Move"; distanceStr ] ->
          result {
            let! distance = validateDistance distanceStr
            let command = Move distance
            turtleAgent.Post command
          }
        | [ "Turn"; angleStr ] ->
          result {
            let! angle = validateAngle angleStr
            let command = Turn angle
            turtleAgent.Post command
          }
        | [ "Pen"; "Up" ] ->
          result {
            let command = PenUp
            turtleAgent.Post command
          }
        | [ "Pen"; "Down" ] ->
          result {
            let command = PenDown
            turtleAgent.Post command
          }
        | [ "SetColor"; colorStr ] ->
          result {
            let! color = validateColor colorStr
            let command = SetColor color
            turtleAgent.Post command
          }
        | _ -> Error(InvalidCommand commandStr)

      result

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
      // draw black line
      do! api.Exec "Pen Down"
      do! api.Exec "SetColor Black"
      do! api.Exec "Move 100"
      // move without drawing
      do! api.Exec "Pen Up"
      do! api.Exec "Turn 90"
      do! api.Exec "Move 100"
      do! api.Exec "Turn 90"
      // draw red line
      do! api.Exec "Pen Down"
      do! api.Exec "SetColor Red"
      do! api.Exec "Move 100"
      // move without drawing
      do! api.Exec "Pen Up"
      do! api.Exec "Turn 90"
      do! api.Exec "Move 100"
      do! api.Exec "Turn 90"
      // back home at (0,0) with angle 0
      // draw diagonal blue line
      do! api.Exec "Pen Down"
      do! api.Exec "SetColor Blue"
      do! api.Exec "Turn 45"
      do! api.Exec "Move 100"
    }

  let drawPolygon n =
    let angle = 180.0 - (360.0 / float n)
    let api = TurtleApi()

    // define a function that draws one side
    let drawOneSide () =
      result {
        do! api.Exec "Move 100.0"
        do! api.Exec $"Turn %f{angle}"
      }

    // repeat for all sides
    for i in [ 1..n ] do
      drawOneSide () |> ignore

  let triggerError () =
    let api = TurtleApi()
    api.Exec "InvalidCommand"

TurtleApiClient.drawTriangle ()
TurtleApiClient.drawThreeLines ()
TurtleApiClient.drawPolygon 4
