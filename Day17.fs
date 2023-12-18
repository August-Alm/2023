module AdventOfCode.Day17

open AdventOfCode.GaussianInt
open System.Collections.Generic

[<Struct>]
type Crucible =
  { Pos : GaussianInt
    Dir : GaussianInt
    StraightMoves : int
  }

[<RequireQualifiedAccess>]
module Dir =
  let N = -GaussianInt.ImaginaryOne
  let S = GaussianInt.ImaginaryOne
  let E = GaussianInt.One
  let W = -GaussianInt.One

type Grid = int array array

[<RequireQualifiedAccess>]
module Grid =

  let parse (lines : string array) : Grid =
    Array.init lines.Length (fun y ->
      Array.init lines[y].Length (fun x -> int (lines[y][x]) - int '0'))
  
  let inRange (grid : Grid) (pos : GaussianInt) =
    pos.Y >= 0 && pos.Y < grid.Length &&
    pos.X >= 0 && pos.X < grid[pos.Y].Length

  let getAt (grid : Grid) (pos : GaussianInt) =
    grid[pos.Y][pos.X]
  
  let neighbors minStraight maxStraight grid crucible =
    seq {
      if crucible.StraightMoves < maxStraight then
        let nextPos = crucible.Pos + crucible.Dir
        yield {
          crucible with
            Pos = nextPos; StraightMoves = crucible.StraightMoves + 1 }
      if crucible.StraightMoves >= minStraight then
        let dir = crucible.Dir * GaussianInt.ImaginaryOne
        yield { Pos = crucible.Pos + dir; Dir = dir; StraightMoves = 1 }
        yield { Pos = crucible.Pos - dir; Dir = -dir; StraightMoves = 1 }
    }
    |> Seq.filter (_.Pos >> inRange grid)

  let minHeatLoss minStraight maxStraight (grid : Grid) =
    let startS = { Pos = GaussianInt.Zero; Dir = Dir.S; StraightMoves = 0 }
    let startE = { startS with Dir = Dir.E }
    let isGoal crucible =
      crucible.Pos.Y = grid.Length - 1 &&
      crucible.Pos.X = grid[grid.Length - 1].Length - 1
    let queue = PriorityQueue<Crucible, int> ()
    queue.Enqueue (startE, 0)
    queue.Enqueue (startS, 0)
    let seen = HashSet<Crucible> ()
    let mutable todo = true
    let mutable heatloss = 0
    let mutable crucible = Unchecked.defaultof<Crucible>
    while todo && queue.TryDequeue (&crucible, &heatloss) do
      if isGoal crucible && crucible.StraightMoves >= minStraight then
        todo <- false
      else
        for next in neighbors minStraight maxStraight grid crucible do
          if seen.Add next then
            queue.Enqueue (next, heatloss + getAt grid next.Pos)
    heatloss
  
module Puzzle1 =

  open System.IO

  let solve (path : string) =
    File.ReadAllLines path
    |> Grid.parse
    |> Grid.minHeatLoss 0 3
      
    
module Puzzle2 =

  open System.IO

  let solve (path : string) =
    File.ReadAllLines path
    |> Grid.parse
    |> Grid.minHeatLoss 4 10
      