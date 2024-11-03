#load "Common.fsx"
#load "OOTurtleLib.fsx"

open Common
open OOTurtleLib

module OOTurtleClient =

  let log message = printfn $"%s{message}"

  let drawTriangle () =
    let turtle = Turtle(log)
    turtle.Move 100.0
    turtle.Turn 120.0<Degrees>
    turtle.Move 100.0
    turtle.Turn 120.0<Degrees>
    turtle.Move 100.0
    turtle.Turn 120.0<Degrees>
  // back home at (0,0) with angle 0

  let drawThreeLines () =
    let turtle = Turtle(log)
    // draw black line
    turtle.PenDown()
    turtle.SetColor Black
    turtle.Move 100.0
    // move without drawing
    turtle.PenUp()
    turtle.Turn 90.0<Degrees>
    turtle.Move 100.0
    turtle.Turn 90.0<Degrees>
    // draw red line
    turtle.PenDown()
    turtle.SetColor Red
    turtle.Move 100.0
    // move without drawing
    turtle.PenUp()
    turtle.Turn 90.0<Degrees>
    turtle.Move 100.0
    turtle.Turn 90.0<Degrees>
    // back home at (0,0) with angle 0
    // draw diagonal blue line
    turtle.PenDown()
    turtle.SetColor Blue
    turtle.Turn 45.0<Degrees>
    turtle.Move 100.0

  let drawPolygon n =
    let angle = 180.0 - (360.0 / float n)
    let angleDegrees = angle * 1.0<Degrees>
    let turtle = Turtle(log)

    // define a function that draws one side
    let drawOneSide () =
      turtle.Move 100.0
      turtle.Turn angleDegrees

    // repeat for all sides
    for i in [ 1..n ] do
      drawOneSide ()

OOTurtleClient.drawTriangle()
OOTurtleClient.drawThreeLines()
OOTurtleClient.drawPolygon 4