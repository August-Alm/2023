module AdventOfCode.Day9

open AdventOfCode.Parsec


type History = int list

let rec delta (history : History) =
  match history with
  | [] | [_] -> []
  | x :: y :: xs -> (y - x) :: delta (y :: xs)

let pHistory = pAtLeastOne pInt


module Puzzle1 =

  open System.IO

  let rec complete history =
    if List.forall ((=) 0) history then 0
    else complete (delta history) + List.last history

  let solve (input : string) =
    File.ReadAllLines input
    |> Array.map (getParsed pHistory)
    |> Array.sumBy complete


module Puzzle2 =

  open System.IO

  let rec complete history =
    if List.forall ((=) 0) history then 0
    else List.head history - complete (delta history)

  let solve (input : string) =
    File.ReadAllLines input
    |> Array.map (getParsed pHistory)
    |> Array.sumBy complete