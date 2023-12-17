module AdventOfCode.Day16


[<Struct>]
type V2 = { X : int; Y : int }
with
  static member Zero = { X = 0; Y = 0 }
  static member E1 = { X = 1; Y = 0 }
  static member E2 = { X = 0; Y = 1 }

  static member (+) (a, b) = { X = a.X + b.X; Y = a.Y + b.Y }
  
  static member (-) (a, b) = { X = a.X - b.X; Y = a.Y - b.Y }
  
  static member (~-) a = { X = -a.X; Y = -a.Y }

type Dir = Left | Right | Up | Down
with
  static member toV2 =
    function
    | Left -> -V2.E1
    | Right -> V2.E1
    | Up -> -V2.E2
    | Down -> V2.E2

  static member (+) (a : V2, b : Dir) =
    a + Dir.toV2 b
  
  static member (-) (a : V2, b : Dir) =
    a - Dir.toV2 b

  static member (~-) =
    function
    | Left -> Right
    | Right -> Left
    | Up -> Down
    | Down -> Up

type Grid = Map<V2, char>

[<RequireQualifiedAccess>]
module Grid =

  let ctor (lines : string array) =
    Map.ofSeq (seq {
      for y in 0 .. lines.Length - 1 do
        for x in 0 .. lines[y].Length - 1 do
          yield ({ X = x; Y = y }, lines[y][x]) })

  let exits (grid : Grid) (pos : V2, dir : Dir) =
    match Map.tryFind pos grid with
    | Some '-' when dir = Up || dir = Down -> [ Left; Right ]
    | Some '|' when dir = Left || dir = Right -> [ Up; Down ]
    | Some '/' ->
      match dir with
      | Left -> [ Down ]
      | Right -> [ Up ]
      | Up -> [ Right ]
      | Down -> [ Left ]
    | Some '\\' ->
      match dir with
      | Left -> [ Up ]
      | Right -> [ Down ]
      | Up -> [ Left ]
      | Down -> [ Right ]
    | Some _ -> [ dir ]
    | None -> []
    |> Seq.choose (fun dir ->
      if Map.containsKey (pos + dir) grid then Some (pos + dir, dir) else None)
  

  let trace (pos : V2) (dir : Dir) (grid : Grid) =
    let rec visit (seen : Set<V2 * Dir>) (pos, dir) =
      Seq.fold visitExits (Set.add (pos, dir) seen) (exits grid (pos, dir))
    and visitExits seen (pos, dir) =
      if not (Set.contains (pos, dir) seen) then visit seen (pos, dir) else seen
    visit Set.empty (pos, dir)
  
  let energized (pos : V2, dir : Dir) (grid : Grid) =
    trace pos dir grid 
    |> Set.map fst
    |> Set.count

  let startBeams (grid : Grid) =
    let br = Seq.maxBy (fun { X = x; Y = y } -> x + y) grid.Keys
    [ yield! grid.Keys |> Seq.filter (_.X >> (=) 0) |> Seq.map (fun p -> (p, Down))
      yield! grid.Keys |> Seq.filter (_.X >> (=) br.X) |> Seq.map (fun p -> (p, Left))
      yield! grid.Keys |> Seq.filter (_.Y >> (=) 0) |> Seq.map (fun p -> (p, Right))
      yield! grid.Keys |> Seq.filter (_.Y >> (=) br.Y) |> Seq.map (fun p -> (p, Up))
    ]

module Puzzle1 =

  open System.IO

  let solve (path : string) =
    File.ReadAllLines path
    |> Grid.ctor
    |> Grid.energized ({ X = 0; Y = 0 }, Right)

module Puzzle2 =

  open System.IO

  let solve (path : string) =
    let grid = Grid.ctor (File.ReadAllLines path)
    Grid.startBeams grid
    |> Seq.map (fun (pos, dir) -> Grid.energized (pos, dir) grid)
    |> Seq.max