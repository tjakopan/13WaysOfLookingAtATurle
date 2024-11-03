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

module EventProcessors =
  let turtleFilter ev =
    match box ev with
    | :? TurtleEvent as tev -> Some tev
    | _ -> None

  let moveFilter =
    function
    | MovedEvent ev -> Some ev
    | _ -> None

  let stateChangedEventFilter =
    function
    | StateChangedEvent ev -> Some ev
    | _ -> None

  let physicalTurtleProcessor (eventStream: IObservable<Guid * obj>) =
    let subscriberFn (ev: MovedEvent) =
      let colorText =
        match ev.penColor with
        | Some color -> $"line of color %A{color}"
        | None -> "no line"

      printfn
        $"[turtle ]: Moved from (%0.2f{ev.startPos.x}, %0.2f{ev.startPos.y}) to (%0.2f{ev.endPos.x}, %0.2f{ev.endPos.y}) with %s{colorText}"

    eventStream
    |> Observable.choose (function
      | _, ev -> turtleFilter ev)
    |> Observable.choose moveFilter
    |> Observable.subscribe subscriberFn

  let graphicsProcessor (eventStream: IObservable<Guid * obj>) =
    let subscriberFn (ev: MovedEvent) =
      match ev.penColor with
      | Some color ->
        printfn
          $"[graphics]: Draw line from (%0.2f{ev.startPos.x}, %0.2f{ev.startPos.y}) to (%0.2f{ev.endPos.x}, %0.2f{ev.endPos.y}) with color %A{color}"
      | None -> ()

    eventStream
    |> Observable.choose (function
      | _, ev -> turtleFilter ev)
    |> Observable.choose moveFilter
    |> Observable.subscribe subscriberFn

  let inkUsedProcessor (eventSteam: IObservable<Guid * obj>) =
    let accumulate (prevDist, currDist) (ev: StateChangedEvent) =
      let newDist =
        match ev with
        | Moved dist -> currDist + dist
        | _ -> currDist

      (currDist, newDist)

    let changedDistanceOnly (currDist, newDist) =
      if currDist <> newDist then Some newDist else None

    let subscriberFn distanceSoFar =
      printfn $"[ink used]: %0.2f{distanceSoFar}"

    eventSteam
    |> Observable.choose (function
      | _, ev -> turtleFilter ev)
    |> Observable.choose stateChangedEventFilter
    |> Observable.scan accumulate (0.0, 0.0)
    |> Observable.choose changedDistanceOnly
    |> Observable.subscribe subscriberFn

module CommandHandlerClient =
  open CommandHandler

  let eventStore = EventStore()

  let makeCommandHandler =
    let logger = ignore
    let getEvents id = eventStore.Get<TurtleEvent>(id)

    let getStateChangedEvents id =
      getEvents id
      |> List.choose (function
        | StateChangedEvent ev -> Some ev
        | _ -> None)

    let saveEvent id ev = eventStore.Save(id, ev)
    commandHandler logger getStateChangedEvents saveEvent

  let turtleId = System.Guid.NewGuid()

  let move dist =
    { turtleId = turtleId
      action = Move dist }

  let turn angle =
    { turtleId = turtleId
      action = Turn angle }

  let penDown =
    { turtleId = turtleId
      action = PenDown }

  let penUp = { turtleId = turtleId; action = PenUp }

  let setColor color =
    { turtleId = turtleId
      action = SetColor color }

  let drawTriangle () =
    eventStore.Clear turtleId
    let eventStream = eventStore.SaveEvent :> IObservable<Guid * obj>
    use physicalTurtleProcessor = EventProcessors.physicalTurtleProcessor eventStream
    use graphicsProcessor = EventProcessors.graphicsProcessor eventStream
    use inkUsedProcessor = EventProcessors.inkUsedProcessor eventStream

    let handler = makeCommandHandler
    handler (move 100.0)
    handler (turn 120.0<Degrees>)
    handler (move 100.0)
    handler (turn 120.0<Degrees>)
    handler (move 100.0)
    handler (turn 120.0<Degrees>)

  let drawThreeLines () =
    eventStore.Clear turtleId

    let eventStream = eventStore.SaveEvent :> IObservable<Guid * obj>

    use physicalTurtleProcessor = EventProcessors.physicalTurtleProcessor eventStream
    use graphicsProcessor = EventProcessors.graphicsProcessor eventStream
    use inkUsedProcessor = EventProcessors.inkUsedProcessor eventStream

    let handler = makeCommandHandler
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
    eventStore.Clear turtleId

    let eventStream = eventStore.SaveEvent :> IObservable<Guid * obj>

    use physicalTurtleProcessor = EventProcessors.physicalTurtleProcessor eventStream
    use graphicsProcessor = EventProcessors.graphicsProcessor eventStream
    use inkUsedProcessor = EventProcessors.inkUsedProcessor eventStream

    let angle = 180.0 - (360.0 / float n)
    let angleDegrees = angle * 1.0<Degrees>
    let handler = makeCommandHandler

    let drawOneSide sideNumber =
      handler (move 100.0)
      handler (turn angleDegrees)

    for i in [ 1..n ] do
      drawOneSide i

CommandHandlerClient.drawTriangle ()
CommandHandlerClient.drawThreeLines ()
CommandHandlerClient.drawPolygon 4
