module AdventOfCode.Day22

open System.Collections.Generic

[<Struct>]
type Range = { Min: int; Max: int }

type Block = { X : Range; Y : Range; Z : Range }
with
  member this.Top = this.Z.Max
  member this.Bottom = this.Z.Min

type Supports =
  { Above : Dictionary<Block, HashSet<Block>>
    Below : Dictionary<Block, HashSet<Block>>
  }

[<RequireQualifiedAccess>]
module Block =

  let parse (lines : string array) =
    lines |> Array.map (fun line ->
      let ns = line.Split ('~', ',')
      { X = { Min = int ns[0]; Max = int ns[3] }
        Y = { Min = int ns[1]; Max = int ns[4] }
        Z = { Min = int ns[2]; Max = int ns[5] }
      })
  
  let private intersects (r1 : Range) (r2 : Range) =
    r1.Min <= r2.Max && r2.Min <= r1.Max
  
  let intersectsXY (b1 : Block) (b2 : Block) =
    intersects b1.X b2.X && intersects b1.Y b2.Y

[<RequireQualifiedAccess>]
module Supports =

  let get (blocks : Block array) =
    let init = Seq.map (fun b -> KeyValuePair (b, HashSet<Block> ()))
    let above = Dictionary (init blocks)
    let below = Dictionary (init blocks)
    for i = 0 to blocks.Length - 1 do
      for j = i + 1 to blocks.Length - 1 do
        let zNbs = blocks[j].Bottom = 1 + blocks[i].Top
        if zNbs && Block.intersectsXY blocks[i] blocks[j] then
          below[blocks[j]].Add blocks[i] |> ignore
          above[blocks[i]].Add blocks[j] |> ignore
    { Above = above; Below = below }

let fall (blocks : Block array) =
  let blocks = blocks |> Array.sortBy _.Bottom
  for i = 0 to blocks.Length - 1 do
    let mutable newBottom = 1
    for j = 0 to i - 1 do
      if Block.intersectsXY blocks[i] blocks[j] then
        newBottom <- max newBottom (blocks[j].Top + 1)
    let f = blocks[i].Bottom - newBottom
    blocks[i] <-
      { blocks[i] with
          Z.Min = blocks[i].Z.Min - f
          Z.Max = blocks[i].Z.Max - f
      }
  blocks

let kaboom (blocks : Block array) =
  let supports = Supports.get blocks
  blocks |> Seq.map (fun desintegrated ->
    let q = Queue<Block> ()
    q.Enqueue desintegrated
    let falling = HashSet<Block> ()
    while q.Count > 0 do
      let block = q.Dequeue ()
      falling.Add block |> ignore
      supports.Above[block]
      |> Seq.filter (fun b -> supports.Below[b].IsSubsetOf falling)
      |> Seq.iter q.Enqueue
    falling.Count - 1)
  

module Puzzle1 =

  open System.IO

  let solve (input : string) =
    File.ReadAllLines input
    |> Block.parse
    |> fall
    |> kaboom
    |> Seq.sumBy (fun x -> if x = 0 then 1 else 0)


module Puzzle2 =

  open System.IO

  let solve (input : string) =
    File.ReadAllLines input
    |> Block.parse
    |> fall
    |> kaboom
    |> Seq.sum