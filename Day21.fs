module AdventOfCode.Day21


[<RequireQualifiedAccess>]
module Seq =

  let rec iterate (f : 'a -> 'a) (x : 'a) =
    let y = f x in seq { yield x; yield! iterate f y }


type Pos = { X : bigint; Y : bigint }

type Grid = { Positions : Set<Pos>; Size : bigint }

[<RequireQualifiedAccess>]
module Grid =

  let private coords (lines : string seq) (c : char) =
    Seq.collect
      (fun (y : int, line) ->
        Seq.map
          (fun (x : int, _) -> { X = bigint x; Y = bigint y })
          (Seq.filter (fun (_, c') -> c = c') (Seq.indexed line)))
      (Seq.indexed lines)
  
  let parse (lines : string seq) =
    let positions = coords lines '.'
    let start = Seq.exactlyOne (coords lines 'S')
    let n = Seq.length lines
    let set = Set.ofSeq (Seq.append [start] positions)
    start, { Positions = set; Size = bigint n }
  
  let valid (grid : Grid) (pos : Pos) =
    let n = grid.Size
    grid.Positions.Contains { X = (pos.X % n + n) % n; Y = (pos.Y % n + n) % n }
  
  let neighbours (grid : Grid) (pos : Pos) =
    (Set.ofSeq << Seq.filter (valid grid))
      [ { X = pos.X + 1I; Y = pos.Y }
        { X = pos.X - 1I; Y = pos.Y }
        { X = pos.X; Y = pos.Y + 1I }
        { X = pos.X; Y = pos.Y - 1I }
      ]
  
  let visit (grid : Grid) (positions : Set<Pos>) =
    (Set.unionMany << Set.map (neighbours grid)) positions
  
  let walks (start, grid) =
    Seq.iterate (visit grid) (Set.singleton start) |> Seq.cache  


module Puzzle1 =

  open System.IO

  let solve (input : string) =
    File.ReadAllLines input
    |> Grid.parse
    |> Grid.walks
    |> Seq.item 64
    |> Set.count

module Puzzle2 =

  open System.IO

  let solution (steps : int) (start : Pos, grid : Grid) =
    let n = (bigint steps) / grid.Size
    let rem = (bigint steps) % grid.Size
    let walks = Grid.walks (start, grid)
    let coeffs =
      [ rem; grid.Size + rem; grid.Size * 2I + rem ]
      |> List.map (fun i -> Seq.item (int i) walks)
      |> List.map (Set.count >> bigint)
    match coeffs with
    [ s0; s1; s2 ] ->
      let c = s0
      let a_plus_b = s1 - s0
      let four_a_plus_two_b = s2 - s0
      let two_a = four_a_plus_two_b - 2I * a_plus_b
      let a = two_a / 2I
      let b = a_plus_b - a
      a * (n * n) + b * n + c
    | _ -> failwith "impossible!"
    

  let solve (input : string) =
    File.ReadAllLines input
    |> Grid.parse 
    |> solution 26501365