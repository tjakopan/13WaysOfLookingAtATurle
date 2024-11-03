open System
open Microsoft.FSharp.Core

type Distance = float

[<Measure>]
type Degrees

type Angle = float<Degrees>

type PenState =
  | Up
  | Down

type PenColor =
  | Black
  | Red
  | Blue

type Position = { x: float; y: float }

let round2 (x: float) = Math.Round(x, 2)

let calcNewPosition (distance: Distance) (angle: Angle) currentPos =
  let angleInRads = angle * (Math.PI / 180.0) * 1.0<1 / Degrees>
  let x0 = currentPos.x
  let y0 = currentPos.y
  let x1 = x0 + distance * Math.Cos(angleInRads)
  let y1 = y0 + distance * Math.Sin(angleInRads)
  { x = round2 x1; y = round2 y1 }

let initialPosition, initialColor, initialPenState =
  { x = 0.0; y = 0.0 }, Black, Down

let dummyDrawLine log oldPos newPos color =
  log $"Drawing line from (%0.1f{oldPos.x}, %0.1f{oldPos.y}) to (%0.1f{newPos.x}, %0.1f{newPos.y}) with color %A{color}"

let trimString (str: string) = str.Trim()

module Result =
  let returnR x = Ok x

  // infix version of bind
  let (>>=) xR f = Result.bind f xR

  // infix version of map
  let (<!>) = Result.map

  let applyR fR xR =
    fR >>= (fun f -> xR >>= (fun x -> returnR (f x)))

  // infix version of apply
  let (<*>) = applyR

  let lift1R f x = f <!> x

  let lift2R f x y = f <!> x <*> y

  type ResultBuilder() =
    member this.Bind(m: Result<'a, 'error>, f: 'a -> Result<'b, 'error>) = Result.bind f m
    member this.Return(x) : Result<'a, 'error> = returnR x
    member this.ReturnFrom(x) : Result<'a, 'error> = x
    member this.Zero() : Result<unit, 'error> = this.Return()
    member this.Combine(m1, f) = this.Bind(m1, f)
    member this.Delay(f) = f
    member this.Run(f) = f ()

    member this.TryWith(m: Result<'a, 'error>, h: exn -> Result<'a, 'error>) =
      try
        this.ReturnFrom(m)
      with e ->
        h e

    member this.TryFinally(m: Result<'a, 'error>, compensation) =
      try
        this.ReturnFrom(m)
      finally
        compensation ()

    member this.Using(res: #IDisposable, body) : Result<'b, 'error> =
      this.TryFinally(
        body res,
        (fun () ->
          match res with
          | null -> ()
          | disp -> disp.Dispose())
      )

    member this.While(cond, m) =
      if not (cond ()) then
        this.Zero()
      else
        this.Bind(m (), (fun _ -> this.While(cond, m)))

    member this.For(sequence: seq<_>, body) =
      this.Using(sequence.GetEnumerator(), (fun enum -> this.While(enum.MoveNext, (fun _ -> body enum.Current))))

  let result = ResultBuilder()
