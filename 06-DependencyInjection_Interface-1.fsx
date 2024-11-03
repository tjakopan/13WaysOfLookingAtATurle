#load "Common.fsx"
#load "OOTurtleLib.fsx"

open Common

type ITurtle =
  abstract Move: Distance -> unit
  abstract Turn: Angle -> unit
  abstract PenUp: unit -> unit
  abstract PenDown: unit -> unit
  abstract SetColor: PenColor -> unit

module TurtleApiLayer_OO =
  exception TurtleApiException of string

  type TurtleApi(turtle: ITurtle) =
    let validateDistance distanceStr =
      try
        float distanceStr
      with ex ->
        let msg = $"Invalid distance '%s{distanceStr}': %s{ex.Message}"
        raise (TurtleApiException msg)

    let validateAngle angleStr =
      try
        (float angleStr) * 1.0<Degrees>
      with ex ->
        let msg = $"Invalid angle '%s{angleStr}': %s{ex.Message}"
        raise (TurtleApiException msg)

    let validateColor colorStr =
      match colorStr with
      | "Black" -> Black
      | "Red" -> Red
      | "Blue" -> Blue
      | _ ->
        let msg = $"Invalid color '%s{colorStr}'"
        raise (TurtleApiException msg)

    member this.Exec(commandStr: string) =
      let tokens = commandStr.Split(' ') |> List.ofArray |> List.map trimString

      match tokens with
      | [ "Move"; distanceStr ] ->
        let distance = validateDistance distanceStr
        turtle.Move distance
      | [ "Turn"; angleStr ] ->
        let angle = validateAngle angleStr
        turtle.Turn angle
      | [ "Pen"; "Up" ] -> turtle.PenUp()
      | [ "Pen"; "Down" ] -> turtle.PenDown()
      | [ "SetColor"; colorStr ] ->
        let color = validateColor colorStr
        turtle.SetColor color
      | _ ->
        let msg = $"Invalid command '%s{commandStr}'"
        raise (TurtleApiException msg)

open TurtleApiLayer_OO

module TurtleImplementation_OO =
  open OOTurtleLib

  let normalSize () =
    let log = printfn "%s"
    let turtle = Turtle(log)

    { new ITurtle with
        member this.Move dist = turtle.Move dist
        member this.Turn angle = turtle.Turn angle
        member this.PenUp() = turtle.PenUp()
        member this.PenDown() = turtle.PenDown()
        member this.SetColor color = turtle.SetColor color }

  let halfSize () =
    let normalSize = normalSize ()

    { new ITurtle with
        member this.Move dist = normalSize.Move(dist / 2.0)
        member this.Turn angle = normalSize.Turn angle
        member this.PenUp() = normalSize.PenUp()
        member this.PenDown() = normalSize.PenDown()
        member this.SetColor color = normalSize.SetColor color }

module TurtleApiClient_OO =

  let drawTriangle (api: TurtleApi) =
    api.Exec "Move 100"
    api.Exec "Turn 120"
    api.Exec "Move 100"
    api.Exec "Turn 120"
    api.Exec "Move 100"
    api.Exec "Turn 120"

do
  let iTurtle = TurtleImplementation_OO.normalSize ()
  let api = TurtleApi(iTurtle)
  TurtleApiClient_OO.drawTriangle api

do
  let iTurtle = TurtleImplementation_OO.halfSize ()
  let api = TurtleApi(iTurtle)
  TurtleApiClient_OO.drawTriangle api
