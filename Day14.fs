module AdventOfCode.Day14

open System.IO

  
[<RequireQualifiedAccess>]
module Array =

  let takeWhileSplit (p : 'a -> bool) (xs : 'a array) =
    let rec loop i =
      if i < xs.Length && p xs[i] then loop (i + 1)
      else (Array.take i xs, Array.skip i xs)
    loop 0

[<Struct>]
type Rock = Cube | Ball

type Platform = (Rock voption) array array

[<RequireQualifiedAccess>]
module Platform =

  let equal (a : Platform) (b : Platform) =
    Array.forall2 (Array.forall2 (=)) a b

  let rec private foo (row : (Rock voption) array) =
    if Array.isEmpty row then
      row
    else
      let a, b = Array.takeWhileSplit ((<>) (ValueSome Cube)) row
      let c, d = Array.takeWhileSplit ((=) (ValueSome Cube)) b
      Array.concat [ Array.sortDescending a; c; foo d ]

  let moveWest : Platform -> Platform  =
    Array.map foo

  let moveNorth : Platform -> Platform =
    Array.transpose << moveWest << Array.transpose
  
  let moveEast : Platform -> Platform =
    Array.map (Array.rev << foo << Array.rev)

  let moveSouth : Platform -> Platform =
    Array.transpose << moveEast << Array.transpose
  
  let cycle1 : Platform -> Platform =
    moveEast << moveSouth << moveWest << moveNorth
  
  let cycle : Platform -> Platform =
    let rec loop n prev xs =
      let ys = cycle1 xs
      match Array.tryFindIndex (equal ys) prev with
      | None ->
        let prev' = Array.concat [ prev; [| ys |]]
        loop n prev' ys
      | Some i ->
        let cycleLength = Array.length prev - i
        let r = (n - 1) % cycleLength
        Array.get prev (r - 1 + i)
    loop 1000000000 [||]
  
  let load : Platform -> int =
    let countdown xs = Array.zip [| Array.length xs .. -1 .. 1 |] xs
    let f (i, r) = i * Array.length (Array.filter ((=) (ValueSome Ball)) r)
    Array.sumBy f << countdown
  
  let parse (input : string) =
    let parseRow (line : string) =
      let parseChar c =
        match c with
        | '.' -> ValueNone
        | '#' -> ValueSome Cube
        | 'O' -> ValueSome Ball
        | _ -> failwith "invalid input"
      Array.map parseChar (line.ToCharArray ())
    File.ReadAllLines input
    |> Array.map parseRow

module Puzzle1 =

  let solve (input : string) =
    Platform.parse input
    |> Platform.moveNorth
    |> Platform.load

module Puzzle2 =
  
    let solve (input : string) =
      Platform.parse input
      |> Platform.cycle
      |> Platform.load