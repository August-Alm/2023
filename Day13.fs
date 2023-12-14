module AdventOfCode.Day13

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


[<Struct>]
type GaussianInt = { X : int; Y : int }
with
  static member Zero = { X = 0; Y = 0 }
  static member One = { X = 1; Y = 0 }
  static member ImaginaryOne = { X = 0; Y = 1 }

  static member (+) (a, b) =
    { X = a.X + b.X; Y = a.Y + b.Y }
  
  static member (-) (a, b) =
    { X = a.X - b.X; Y = a.Y - b.Y }
  
  static member (~-) a =
    { X = -a.X; Y = -a.Y }
  
  static member (*) (a, b) =
    { X = a.X * b.X - a.Y * b.Y; Y = a.X * b.Y + a.Y * b.X }
  
  static member (/) (a, b) =
    failwith "gaussian integers only form a ring"


[<RequireQualifiedAccess>]
module Direction =

  let E1 = GaussianInt.One
  let E2 = GaussianInt.ImaginaryOne

  let ortho dir = if dir = E1 then E2 else E1

type Pattern = Ash | Mirror

type PatternMap = FrozenDictionary<GaussianInt, Pattern>

[<RequireQualifiedAccess>]
module PatternMap =
  
  let ctor (block : string) =
    let lines = String.lines block
    FrozenDictionary.ToFrozenDictionary (seq {
      for y in 0 .. lines.Length - 1 do
        for x in 0 .. lines[y].Length - 1 do
          match lines[y][x] with
          | '.' -> yield KeyValuePair ({ X = x; Y = y }, Ash)
          | '#' -> yield KeyValuePair ({ X = x; Y = y }, Mirror)
          | c -> failwithf "not a pattern character: %c" c })
  
  let patternAt (map : PatternMap) (pos : GaussianInt) =
    match map.TryGetValue pos with
    | true, v -> Some v
    | _ -> None

  let getRay (map : PatternMap) (start : GaussianInt) (dir : GaussianInt) =
    start |> List.unfold (fun pos ->
      if map.ContainsKey pos then Some (pos, pos + dir) else None)
  
  let findSmudges (map : PatternMap) (mirror : GaussianInt) (dir : GaussianInt) =
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
            yield mirror.X + 100 * mirror.Y })  

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