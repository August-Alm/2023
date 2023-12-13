module AdventOfCode.Day11

open System.IO

type Galaxy = int * int

[<RequireQualifiedAccess>]
module Galaxy =

  let expand (by : int) (galaxies : Galaxy list) =
    let xs = Set.ofList (List.map snd galaxies)
    let ys = Set.ofList (List.map fst galaxies)
    let missingXs =
      Seq.init (Seq.max xs + 1) id
      |> Seq.filter (fun x -> not (Set.contains x xs))
      |> Seq.cache
    let missingYs =
      Seq.init (Seq.max ys + 1) id
      |> Seq.filter (fun y -> not (Set.contains y ys))
      |> Seq.cache
    let adjustX x =
      Seq.tryFindIndex ((<) x) missingXs
      |> Option.defaultValue (Seq.length missingXs)
    let adjustY y =
      Seq.tryFindIndex ((<) y) missingYs
      |> Option.defaultValue (Seq.length missingYs)
    let f (y, x) =
      (y + by * adjustY y, x + by * adjustX x)
    List.map f galaxies
  
  let pairs (galaxies : Galaxy list) =
    let f i = List.map (fun g -> (galaxies[i], g)) (List.skip (i + 1) galaxies)
    List.collect f [ 0 .. List.length galaxies - 1 ]
  
  let distance (y1, x1) (y2, x2) =
    abs (y1 - y2) + abs (x1 - x2)
  
  let sumDistances (galaxies : Galaxy list) =
    List.sumBy (fun (g1, g2) -> int64 (distance g1 g2)) (pairs galaxies)

  let parse (input : string) =
    let row (y, line) =
      Seq.fold (fun gs (x, c) -> if c = '#' then (y, x) :: gs else gs)
        [] (Seq.indexed line)
    File.ReadAllLines input
    |> Seq.indexed
    |> Seq.collect row
    |> Seq.toList
  

module Puzzle1 =

  let solve (input : string) =
    Galaxy.parse input
    |> Galaxy.expand 1
    |> Galaxy.sumDistances

module Puzzle2 =

  let solve (input : string) =
    Galaxy.parse input
    |> Galaxy.expand 999999
    |> Galaxy.sumDistances