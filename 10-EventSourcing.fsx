#load "Common.fsx"
#load "FPTurtleLib.fsx"

open System
open System.Collections.Generic
open Common
open FPTurtleLib

type EventStore() =
  let eventDict = Dictionary<Guid, obj list>()

  let saveEvent = Event<Guid * obj>()

  member this.SaveEvent = saveEvent.Publish

  member this.Save(eventId, event) =
    match eventDict.TryGetValue eventId with
    | true, eventList ->
      let newList = event :: eventList
      eventDict[eventId] <- newList
    | false, _ ->
      let newList = [ event ]
      eventDict[eventId] <- newList

    saveEvent.Trigger(eventId, event)

  member this.Get<'a>(eventId) =
    match eventDict.TryGetValue eventId with
    | true, eventList -> eventList |> Seq.cast<'a> |> Seq.toList |> List.rev
    | false, _ -> []

  member this.Clear(eventId) = eventDict[eventId] <- []

type TurtleId = Guid

type TurtleCommandAction =
  | Move of Distance
  | Turn of Angle
  | PenUp
  | PenDown
  | SetColor of PenColor

type TurtleCommand =
  { turtleId: TurtleId
    action: TurtleCommandAction }

type StateChangedEvent =
  | Moved of Distance
  | Turned of Angle
  | PenWentUp
  | PenWentDown
  | ColorChanged of PenColor

type MovedEvent =
  { startPos: Position
    endPos: Position
    penColor: PenColor option }

type TurtleEvent =
  | StateChangedEvent of StateChangedEvent
  | MovedEvent of MovedEvent

module CommandHandler =
  let applyEvent log oldState event =
    match event with
    | Moved distance -> Turtle.move log distance oldState
    | Turned angle -> Turtle.turn log angle oldState
    | PenWentUp -> Turtle.penUp log oldState
    | PenWentDown -> Turtle.penDown log oldState
    | ColorChanged color -> Turtle.setColor log color oldState

  let eventsFromCommand log command stateBeforeCommand =
    let stateChangedEvent =
      match command.action with
      | Move dist -> Moved dist
      | Turn angle -> Turned angle
      | PenUp -> PenWentUp
      | PenDown -> PenWentDown
      | SetColor color -> ColorChanged color

    let stateAfterCommand = applyEvent log stateBeforeCommand stateChangedEvent
    let startPos = stateBeforeCommand.position
    let endPos = stateAfterCommand.position

    let penColor =
      if stateBeforeCommand.penState = Down then
        Some stateBeforeCommand.color
      else
        None

    let movedEvent =
      { startPos = startPos
        endPos = endPos
        penColor = penColor }

    if startPos <> endPos then
      [ StateChangedEvent stateChangedEvent; MovedEvent movedEvent ]
    else
      [ StateChangedEvent stateChangedEvent ]

  type GetStateChangedEventsForId = TurtleId -> StateChangedEvent list

  type SaveTurtleEvent = TurtleId -> TurtleEvent -> unit

  let commandHandler
    (log: string -> unit)
    (getEvents: GetStateChangedEventsForId)
    (saveEvent: SaveTurtleEvent)
    (command: TurtleCommand)
    =
    let eventHistory = getEvents command.turtleId

    let stateBeforeCommand =
      let nolog = ignore // no logging when re-creating state
      eventHistory |> List.fold (applyEvent nolog) Turtle.initialTurtleState

    let events = eventsFromCommand log command stateBeforeCommand
    events |> List.iter (saveEvent command.turtleId)

module CommandHandlerClient =
  open CommandHandler

  let stateChangedEventFilter =
    function
    | StateChangedEvent ev -> Some ev
    | _ -> None

  let makeCommandHandler () =
    let logger = printfn "%s"
    let eventStore = EventStore()

    let getStateChangedEvents id =
      eventStore.Get<TurtleEvent>(id) |> List.choose stateChangedEventFilter

    let saveEvent id ev = eventStore.Save(id, ev)
    commandHandler logger getStateChangedEvents saveEvent

  let turtleId = Guid.NewGuid()

  let move dist =
    { turtleId = turtleId
      action = Move dist }

  let turn angle =
    { turtleId = turtleId
      action = Turn angle }

  let penUp = { turtleId = turtleId; action = PenUp }

  let penDown =
    { turtleId = turtleId
      action = PenDown }

  let setColor color =
    { turtleId = turtleId
      action = SetColor color }

  let drawTriangle () =
    let handler = makeCommandHandler ()
    handler (move 100.0)
    handler (turn 120.0<Degrees>)
    handler (move 100.0)
    handler (turn 120.0<Degrees>)
    handler (move 100.0)
    handler (turn 120.0<Degrees>)

  let drawThreeLines () =
    let handler = makeCommandHandler ()
    handler penDown
    handler (setColor Black)
    handler (move 100.0)

    handler penUp
    handler (turn 90.0<Degrees>)
    handler (move 100.0)
    handler (turn 90.0<Degrees>)

    handler penDown
    handler (setColor Red)
    handler (move 100.0)

    handler penUp
    handler (turn 90.0<Degrees>)
    handler (move 100.0)
    handler (turn 90.0<Degrees>)

    handler penDown
    handler (setColor Blue)
    handler (turn 45.0<Degrees>)
    handler (move 100.0)

  let drawPolygon n =
    let angle = 180.0 - (360.0 / float n)
    let angleDegrees = angle * 1.0<Degrees>
    let handler = makeCommandHandler ()

    let drawOneSide sideNumber =
      handler (move 100.0)
      handler (turn angleDegrees)

    for i in [ 1..n ] do
      drawOneSide i

CommandHandlerClient.drawTriangle ()
CommandHandlerClient.drawThreeLines ()
CommandHandlerClient.drawPolygon 4
