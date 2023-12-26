module AdventOfCode.Day23

open System.Collections.Generic

[<RequireQualifiedAccess>]
module Map =

  let union (m1 : Map<'k, 'v>) (m2 : Map<'k, 'v>) =
    Map.fold (fun m k v -> Map.add k v m) m1 m2
  
  let unions (maps : Map<'k, 'v> list) =
    List.reduce union maps


[<Struct>]
type Pos = { X : int; Y : int }

[<Struct;RequireQualifiedAccess>]
type Dir = U | D | L | R
with
  static member all = [ U; D; L; R ]

  static member (+) (p : Pos, d : Dir) =
    match d with
    | U -> { X = p.X; Y = p.Y - 1 }
    | D -> { X = p.X; Y = p.Y + 1 }
    | L -> { X = p.X - 1; Y = p.Y }
    | R -> { X = p.X + 1; Y = p.Y }
  
  static member (~-) (d : Dir) =
    match d with
    | U -> D | D -> U
    | L -> R | R -> L

type Tile =
  | Path
  | Forest
  | Slope of Dir

type Grid = Tile array2d

[<RequireQualifiedAccess>]
module Grid =

  let parse (lines : string array) =
    Array2D.init lines.Length lines[0].Length
      (fun y x ->
        match lines[y][x] with
        | '.' -> Path
        | '#' -> Forest
        | '^' -> Slope Dir.U
        | 'v' -> Slope Dir.D
        | '<' -> Slope Dir.L
        | '>' -> Slope Dir.R
        | c -> failwith $"illegal grid symbol {c}")

  let contains (grid : Grid) (p : Pos) =
    p.X >= 0 && p.X < Array2D.length2 grid &&
    p.Y >= 0 && p.Y < Array2D.length1 grid
  
  let tryGetAt (grid : Grid) (p : Pos) =
    if contains grid p then Some (Array2D.get grid p.Y p.X) else None

  let isFree (grid : Grid) (p : Pos) =
    match tryGetAt grid p with
    | None | Some Forest -> false
    | _ -> true

  let start (grid : Grid) =
    let x = Array.findIndex ((=) Path) grid[0, 0..]
    { X = x; Y = 0 }
  
  let goal (grid : Grid) =
    let maxY = Array2D.length1 grid - 1
    let x = Array.findIndex ((=) Path) grid[maxY, 0..]
    { X = x; Y = maxY }

  let neighbors (grid : Grid) (p : Pos) =
    let go d = if isFree grid (p + d) then Some (p + d) else None
    match tryGetAt grid p with
    | None | Some Forest -> []
    | Some (Slope d) -> List.choose go [ d ]
    | Some Path -> List.choose go Dir.all 


type Graph = 
  { Adjacencies : Map<Pos, (Pos * int) list>
    Start : Pos
    Goal : Pos
  }

[<RequireQualifiedAccess>]
module Graph =

  let ofGrid (grid : Grid) =
    let visited = HashSet<Pos> ()
    let start = Grid.start grid
    let goal = Grid.goal grid
    let rec edge (seen : Set<_>) len p =
      if p = goal then Some (p, len)
      elif seen.Contains p then None
      else
        match List.filter (not << seen.Contains) (Grid.neighbors grid p) with
        | [] -> None
        | [ n ] -> edge (seen.Add p) (len + 1) n
        | _ -> Some (p, len)
    let rec adjacencies p =
      if not (visited.Add p) then
        Map.empty
      else
        match List.choose (edge (Set.singleton p) 1) (Grid.neighbors grid p) with
        | [] -> Map.empty
        | adjs -> Map.add p adjs (Map.unions (List.map (adjacencies << fst) adjs))
    { Adjacencies = adjacencies start; Start = start; Goal = goal }

  let adjacent (graph : Graph) (p : Pos) =
    Option.defaultValue [] (Map.tryFind p graph.Adjacencies)

  let paths (graph : Graph) =
    let visited = Set<Pos> [ graph.Start ]
    let rec helper (visited : Set<_>) u v =
      if u = v then [[]]
      else
        [ for (c, n) in adjacent graph u do
            if not (visited.Contains c) then
              for path in helper (Set.add c visited) c v do
                yield (u, c, n) :: path
        ]
    helper visited graph.Start graph.Goal


module Puzzle1 =

  open System.IO

  let solve (input : string) =
    File.ReadAllLines input
    |> Grid.parse
    |> Graph.ofGrid
    |> Graph.paths
    |> List.map (List.sumBy (fun (_, _, len) -> len))
    |> List.max


module Puzzle2 =

  [<RequireQualifiedAccess>]
  module Grid =

    let parse2 (lines : string array) =
      Array2D.init lines.Length lines[0].Length
        (fun y x ->
          match lines[y][x] with
          | '.' -> Path
          | '#' -> Forest
          | '^' -> Path
          | 'v' -> Path
          | '<' -> Path
          | '>' -> Path
          | c -> failwith $"illegal grid symbol {c}")
  
  open System.IO

  let solve (input : string) =
    File.ReadAllLines input
    |> Grid.parse2
    |> Graph.ofGrid
    |> Graph.paths
    |> List.map (List.sumBy (fun (_, _, len) -> len))
    |> List.max