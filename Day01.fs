module AdventOfCode.Day1

open System

module Puzzle1 =

  let charToDigit (c : char) =
    if c >= '0' && c <= '9' then Some (int c - int '0') else None
  
  let getCalibrationValue (line : string) =
    let mutable first = None
    let mutable second = None
    for c in line do
      match charToDigit c with
      | Some d when first.IsNone -> first <- Some d
      | Some d -> second <- Some d
      | None -> ()
    match first, second with
    | Some f, Some s -> f * 10 + s
    | Some f, None -> f * 10 + f
    | _ -> failwith "Invalid input"

  let solve (input : string) =
    IO.File.ReadAllLines input
    |> Array.sumBy getCalibrationValue


module Puzzle2 =

  type Digit =
    One | Two | Three | Four | Five | Six | Seven | Eight | Nine

  [<RequireQualifiedAccess>]
  module Digit =

    let digits =
      [| One; Two; Three; Four; Five; Six; Seven; Eight; Nine |]
    
    let toInt (d : Digit) =
      match d with
      | One -> 1
      | Two -> 2
      | Three -> 3
      | Four -> 4
      | Five -> 5
      | Six -> 6
      | Seven -> 7
      | Eight -> 8
      | Nine -> 9

    let stringReps (d : Digit) =
      match d with
      | One -> [| "1"; "one" |]
      | Two -> [| "2"; "two" |]
      | Three -> [| "3"; "three" |]
      | Four -> [| "4"; "four" |]
      | Five -> [| "5"; "five" |]
      | Six -> [| "6"; "six" |]
      | Seven -> [| "7"; "seven" |]
      | Eight -> [| "8"; "eight" |]
      | Nine -> [| "9"; "nine" |]
    
    let firstIndexOf (s : string) (r : string) =
      let i = s.IndexOf r
      if i = -1 then None else Some i
    
    let lastIndexOf (s : string) (r : string) =
      let i = s.LastIndexOf r
      if i = -1 then None else Some i

    let firstOccurrence (s : string) (d : Digit) =
      let candidates = Array.choose (firstIndexOf s) (stringReps d)
      if Array.isEmpty candidates then None
      else Some (d, Array.min candidates)

    let lastOccurrence (s : string) (d : Digit) =
      let candidates = Array.choose (lastIndexOf s) (stringReps d)
      if Array.isEmpty candidates then None
      else Some (d, Array.max candidates)
    
    let first (line : string) =
      digits
      |> Array.choose (firstOccurrence line)
      |> Array.minBy snd
      |> fst

    let second (line : string) =
      digits
      |> Array.choose (lastOccurrence line)
      |> Array.maxBy snd
      |> fst


  let getCalibrationValue (line : string) =
    let first = Digit.first line
    let second = Digit.second line
    Digit.toInt first * 10 + Digit.toInt second
  
  let solve (input : string) =
    System.IO.File.ReadAllLines input
    |> Array.sumBy getCalibrationValue
