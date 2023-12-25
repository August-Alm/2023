module AdventOfCode.Day23

open System.Collections.Generic


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
  
  let positions (grid : Grid) =
    seq {
      for y = 0 to Array2D.length1 grid - 1 do
        for x = 0 to Array2D.length2 grid - 1 do
          yield { X = x ; Y = y }
    }

  let tryGetAt (grid : Grid) (p : Pos) =
    if contains grid p then Some (Array2D.get grid p.Y p.X) else None

  let start (grid : Grid) =
    let x = Array.findIndex ((=) Path) grid[0, 0..]
    { X = x; Y = 0 }, Dir.D
  
  let goal (grid : Grid) =
    let maxY = Array2D.length1 grid - 1
    let x = Array.findIndex ((=) Path) grid[maxY, 0..]
    { X = x; Y = maxY }

  let adjacent (slopes : bool) (grid : Grid) (p : Pos) =
    let go (d : Dir) =
      match tryGetAt grid (p + d) with
      | None | Some Forest -> None
      | Some (Slope _) -> if slopes then Some (p + d) else None
      | _ -> Some (p + d)
    match tryGetAt grid p with
    | Some Path -> List.choose go Dir.all
    | Some (Slope d') when slopes -> List.choose go [ d' ]
    | _ -> []
  
  let junctures slopes grid =
    positions grid
    |> Seq.filter (((<>) 2) << List.length << (adjacent slopes grid))
    |> Seq.toArray
  
