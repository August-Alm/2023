module AdventOfCode.Day10

open System.IO

type Dir = N | S | E | W

[<RequireQualifiedAccess>]
module Dir =

  let flip (dir : Dir) =
    match dir with
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

type Grid = Tile array array

[<Struct>]
type Pos = { X : int; Y : int }

[<RequireQualifiedAccess>]
module Pos =

  let go (pos : Pos) (dir : Dir) =
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
  
  let positions (grid : Grid) =
    seq {
      for y in 0 .. grid.Length - 1 do
        for x in 0 .. grid[y].Length - 1 do
          yield { X = x; Y = y }
    }

  let findStart grid =
    positions grid
    |> Seq.find (fun pos -> tileAt grid pos = Some Start)
  
  let next (grid : Grid) (enter : Dir, pos : Pos) =
    match enter, tileAt grid pos with
    | N, Some NS -> Some (N, Pos.go pos S)
    | S, Some NS -> Some (S, Pos.go pos N)
    | E, Some EW -> Some (E, Pos.go pos W)
    | W, Some EW -> Some (W, Pos.go pos E)
    | N, Some NE -> Some (W, Pos.go pos E)
    | E, Some NE -> Some (S, Pos.go pos N)
    | S, Some SW -> Some (E, Pos.go pos W)
    | W, Some SW -> Some (N, Pos.go pos S)
    | N, Some NW -> Some (E, Pos.go pos W)
    | W, Some NW -> Some (S, Pos.go pos N)
    | S, Some SE -> Some (W, Pos.go pos E)
    | E, Some SE -> Some (N, Pos.go pos S)
    | _, _ -> None

  let path (grid : Grid) (stop : Pos) (enter, beginning) =
    let rec loop poses =
      next grid >> Option.bind (fun (dir, pos) ->
        if pos = stop then Some (pos :: poses)
        else loop (pos :: poses) (dir, pos))
    loop [ beginning ] (enter, beginning)
  
  let loop grid =
    let start = findStart grid
    seq { N; S; E; W }
    |> Seq.map (fun enter -> path grid start (Dir.flip enter, Pos.go start enter))
    |> Seq.find Option.isSome
    |> Option.get

let parseGrid (input : string) : Grid =
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
    let path = Grid.loop grid
    path.Length / 2
  
    
module Puzzle2 =

  let inside (grid : Grid) (loop : Set<Pos>) (pos : Pos) =
    let rec foo ans (pos : Pos) =
      match Grid.tileAt grid pos with
      | Some t ->
        match t with
        | NS | NW | NE | Start when loop.Contains pos ->
          foo (not ans) (Pos.go pos W)
        | _ -> foo ans (Pos.go pos W)
      | None -> ans
    if loop.Contains pos then false
    else foo false (Pos.go pos W)

  let solve (input : string) =
    let grid = parseGrid input
    let loop = Set.ofList (Grid.loop grid)
    Grid.positions grid
    |> Seq.filter (inside grid loop)
    |> Seq.length