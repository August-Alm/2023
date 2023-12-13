module AdventOfCode.Day13

open System.Numerics
open System.Collections.Frozen
open System.Collections.Generic
open System.IO
open System


[<RequireQualifiedAccess>]
module String =

  let blocks (s : string) =
    s.Split ([| "\r\n\r\n"; "\n\n" |], StringSplitOptions.RemoveEmptyEntries)
  
  let lines (s : string) =
    s.Split ([| "\r\n"; "\n" |], StringSplitOptions.RemoveEmptyEntries)


[<RequireQualifiedAccess>]
module Direction =

  let E1 = Complex.One
  let E2 = Complex.ImaginaryOne

  let ortho dir = if dir = E1 then E2 else E1

type Pattern = Ash | Mirror

type PatternMap = FrozenDictionary<Complex, Pattern>

[<RequireQualifiedAccess>]
module PatternMap =
  
  let ctor (block : string) =
    let lines = String.lines block
    FrozenDictionary.ToFrozenDictionary (seq {
      for y in 0 .. lines.Length - 1 do
        for x in 0 .. lines[y].Length - 1 do
          match lines[y][x] with
          | '.' -> yield KeyValuePair (Complex (x, y), Ash)
          | '#' -> yield KeyValuePair (Complex (x, y), Mirror)
          | c -> failwithf "not a pattern character: %c" c })
  
  let patternAt (map : PatternMap) (pos : Complex) =
    match map.TryGetValue pos with
    | true, v -> Some v
    | _ -> None

  let getRay (map : PatternMap) (start : Complex) (dir : Complex) =
    start |> List.unfold (fun pos ->
      if map.ContainsKey pos then Some (pos, pos + dir) else None)
  
  let findSmudges (map : PatternMap) (mirror : Complex) (dir : Complex) =
    let inline distinctAtCount (pos1, pos2) =
      if patternAt map pos1 <> patternAt map pos2 then 1 else 0
    List.sum ([
      for ray0 in getRay map mirror (Direction.ortho dir) do
        Seq.zip (getRay map ray0 dir) (getRay map (ray0 - dir) (-dir))
        |> Seq.sumBy distinctAtCount ])

  let getScore (allowedSmudges : int) (map : PatternMap) =
    Seq.head (seq {
      for dir in [ Direction.E1; Direction.E2 ] do
        for mirror in getRay map dir dir do
          if findSmudges map mirror dir = allowedSmudges then
            yield int (mirror.Real + 100.0 * mirror.Imaginary) })  

module Puzzle1 =

  let solve (input : string) =
    File.ReadAllText input
    |> String.blocks
    |> Seq.sumBy (PatternMap.ctor >> PatternMap.getScore 0)

module Puzzle2 =

  let solve (input : string) =
    File.ReadAllText input
    |> String.blocks
    |> Seq.sumBy (PatternMap.ctor >> PatternMap.getScore 1)