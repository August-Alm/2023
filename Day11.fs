module AdventOfCode.Day11

open System.IO

type Galaxy = int * int

[<RequireQualifiedAccess>]
module Galaxy =

  let expand (by : int) (galaxies : Galaxy list) =
    let xs = List.distinct (List.map snd galaxies)
    let ys = List.distinct (List.map fst galaxies)
    let missingXs = List.except xs [ 0 .. List.max xs ]
    let missingYs = List.except ys [ 0 .. List.max ys ]
    let adjustX x =
      List.tryFindIndex ((<) x) missingXs
      |> Option.defaultValue (List.length missingXs)
    let adjustY y =
      List.tryFindIndex ((<) y) missingYs
      |> Option.defaultValue (List.length missingYs)
    galaxies
    |> List.map (fun (y, x) -> (y + by * adjustY y, x + by * adjustX x))
  
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