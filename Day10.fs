module AdventOfCode.Day10

open System.IO

type Dir = N | S | E | W

[<RequireQualifiedAccess>]
module Dir =

  let flip =
    function
    | N -> S
    | S -> N
    | E -> W
    | W -> E


type Tile =
  | NS
  | EW
  | NE
  | NW
  | SW
  | SE
  | Ground
  | Start

[<RequireQualifiedAccess>]
module Tile =

  let openings (tile : Tile) =
    match tile with
    | NS -> [| N; S |]
    | EW -> [| E; W |]
    | NE -> [| N; E |]
    | NW -> [| N; W |]
    | SW -> [| S; W |]
    | SE -> [| S; E |]
    | Ground -> [| |]
    | Start -> [| N; S; E; W |]
  
  let hasOpening (dir : Dir) (tile : Tile) =
    Array.contains dir (openings tile)


type Grid = Tile array array

[<Struct>]
type Pos = { X : int; Y : int }

[<RequireQualifiedAccess>]
module Pos =

  let go (dir : Dir) (pos : Pos) =
    match dir with
    | N -> { pos with Y = pos.Y - 1 }
    | S -> { pos with Y = pos.Y + 1 }
    | E -> { pos with X = pos.X + 1 }
    | W -> { pos with X = pos.X - 1 }


[<RequireQualifiedAccess>]
module Grid =

  let tileAt (grid : Grid) (pos : Pos) =
    if
      0 <= pos.Y && pos.Y < grid.Length &&
      0 <= pos.X && pos.X < grid[pos.Y].Length
    then
      Some (grid[pos.Y][pos.X])
    else
      None
  
  let neighbors (grid : Grid) (pos : Pos) =
    match tileAt grid pos with
    | None -> [||]
    | Some tile -> 
      Tile.openings tile
      |> Array.choose (fun dir ->
        tileAt grid (Pos.go dir pos) |> Option.bind (fun t ->
          if Tile.hasOpening (Dir.flip dir) t then Some (Pos.go dir pos)
          else None))

// Add distances (emanating from the given position) to the map,
// but only if they are shorter than the existing.
let rec foo (grid : Grid) (d : int) (distances : Map<Pos, int>) (pos : Pos) =
  match distances.TryFind pos with
  | Some d' when d' <= d -> distances
  | _ ->
    Array.fold
      (foo grid (d + 1))
      (Map.add pos d distances)
      (Grid.neighbors grid pos)

let getStart (grid : Grid) =
  let mutable y = 0
  let mutable x = 0
  while grid[y].[x] <> Start do
    x <- x + 1
    if x = grid[y].Length then
      x <- 0
      y <- y + 1
  { X = x; Y = y }

let distances (grid : Grid) =
  foo grid 0 Map.empty (getStart grid)

let parseGrid (input : string) =
  use stream = new FileStream (input, FileMode.Open)
  use reader = new StreamReader (stream)
  let rowBuffer = ResizeArray<Tile array> ()
  let lineBuffer = ResizeArray<Tile> ()
  while not reader.EndOfStream do
    for c in reader.ReadLine () do
      match c with
      | '|' -> lineBuffer.Add NS
      | '-' -> lineBuffer.Add EW
      | 'L' -> lineBuffer.Add NE
      | 'J' -> lineBuffer.Add NW
      | '7' -> lineBuffer.Add SW
      | 'F' -> lineBuffer.Add SE
      | '.' -> lineBuffer.Add Ground
      | 'S' -> lineBuffer.Add Start
      | _ -> failwith $"invalid tile character {c}"
    rowBuffer.Add (lineBuffer.ToArray ())
    lineBuffer.Clear ()
  rowBuffer.ToArray ()


module Puzzle1 =

  let solve (input : string) =
    let grid = parseGrid input
    let distances = distances grid
    distances.Values |> Seq.max
  
    
