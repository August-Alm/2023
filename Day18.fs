module AdventOfCode.Day18

open System.Numerics

[<RequireQualifiedAccess; Struct>]
type Dir = U | D | L | R
with
  static member toComplex =
    function
    | U -> Complex.ImaginaryOne
    | D -> -Complex.ImaginaryOne
    | L -> -Complex.One
    | R -> Complex.One
  
  static member (+) (a : Complex, b : Dir) =
    a + Dir.toComplex b
  
  static member (-) (a : Complex, b : Dir) =
    a - Dir.toComplex b
  
  static member (*) (a : Complex, b : Dir) =
    a * Dir.toComplex b
  
  static member (~-) (a : Dir) =
    match a with
    | U -> D | D -> U
    | L -> R | R -> L

[<Struct>]
type Color = { R : byte; G : byte; B : byte }

[<RequireQualifiedAccess>]
module Color =

  open System.Globalization
  open System

  let fromHex (hex : string) =
    let hexDecimal = Int32.Parse (hex, NumberStyles.HexNumber)
    let r = byte ((hexDecimal >>> 16) &&& int Byte.MaxValue)
    let g = byte ((hexDecimal >>> 8) &&& int Byte.MaxValue)
    let b = byte (hexDecimal &&& int Byte.MaxValue)
    { R = r; G = g; B = b }

type DigPlan = { Dir : Dir; Meters : int; Color : Color }

[<RequireQualifiedAccess>]
module DigPlan =
  
  open System.Globalization

  let parse (line : string) =
    let parts = line.Split ' '
    let dir =
      match parts[0] with
      | "U" -> Dir.U
      | "D" -> Dir.D
      | "L" -> Dir.L
      | "R" -> Dir.R
      | d -> failwithf "invalid direction: %s" d
    let meters = int parts[1]
    let color = Color.fromHex (parts[2].Substring (2, 6))
    { Dir = dir; Meters = meters; Color = color }
  
  let trenchStep (plan : DigPlan) =
    Complex (plan.Meters, 0.0) * plan.Dir

  let parseTrueStep (line : string) =
    let part = (line.Split ' ')[2]
    let dir =
      match part[7] with
      | '0' -> Dir.R
      | '1' -> Dir.D
      | '2' -> Dir.L
      | '3' -> Dir.U
      | d -> failwithf "invalid direction: %c" d
    let meters = System.Int32.Parse (part.Substring (2, 5), NumberStyles.HexNumber)
    Complex (meters, 0.0) * dir

  let private vertices (steps : Complex array) =
    let rec f i =
      if i = 0 then Complex.Zero + steps[0]
      else f (i - 1) + steps[i]
    Array.init steps.Length f
  
  let private shoelaces (vertices : Complex array) =
    let shoelace i =
      let p1, p2 =
        if i = vertices.Length - 1 then (vertices[0], vertices[i])
        else (vertices[i], vertices[i + 1])
      p1.Real * p2.Imaginary - p1.Imaginary * p2.Real
    Array.init vertices.Length shoelace
  
  // Pick's theorem
  let area (steps : Complex array) =
    let a = (abs (Array.sum (shoelaces (vertices steps)))) / 2.0
    let boundary =  steps |> Array.sumBy _.Magnitude
    let interior = a - boundary / 2.0 + 1.0
    bigint (boundary + interior)


module Puzzle1 =

  open System.IO

  let solve (input : string) =
    File.ReadAllLines input
    |> Array.map (DigPlan.parse >> DigPlan.trenchStep)
    |> DigPlan.area

module Puzzle2 =
  
    open System.IO
  
    let solve (input : string) =
      File.ReadAllLines input
      |> Array.map DigPlan.parseTrueStep
      |> DigPlan.area
  