module AdventOfCode.Day8

open AdventOfCode.Parsec
open System.Collections.Generic

type Instruction = Left | Right

type Path = Instruction list

[<RequireQualifiedAccess>]
type NodeDescription =
  { Label : string
    Left : string
    Right : string
  }
with
  static member ctor (label : string) (left : string) (right : string) =
    { Label = label; Left = left; Right = right }

type Node (label : string) =
  let mutable left : Node option = None
  let mutable right : Node option = None
  member _.Label = label
  member _.Left
    with get () = left.Value
    and set l = left <- Some l
  member _.Right
    with get () = right.Value
    and set r = right <- Some r

type Network (descriptions : NodeDescription list) =
  let nodes = Dictionary<string, Node> ()
  do
    for d in descriptions do
      nodes.Add (d.Label, Node d.Label)
    for d in descriptions do
      let node = nodes[d.Label]
      node.Left <- nodes[d.Left]
      node.Right <- nodes[d.Right]

  member _.Next (label : string) (instruction : Instruction) =
    let node = nodes[label]
    match instruction with
    | Left -> node.Left.Label
    | Right -> node.Right.Label
  

type Document = { Path : Path; Network : Network }

let pInstruction : Parsec<Instruction> =
  fun input ->
    if input.Length = 0 then None
    else
      match input[0] with
      | 'L' -> Some (Left, input.Substring 1)
      | 'R' -> Some (Right, input.Substring 1)
      | _ -> None

let pPath =
  pAtLeastOne pInstruction

let pNodeDescription =
  let pLabel = pLetters .>> pWord "="
  let pLeft = pChar '(' >>. pLetters
  let pRight = pChar ',' >>. pLetters .>> pChar ')'
  pLabel .>>. pLeft .>>. pRight
  |> map (fun ((label, left), right) -> NodeDescription.ctor label left right)

let pNetwork =
  map Network (pAtLeastOne pNodeDescription)

let pDocument =
  pPath .>>. pNetwork
  |> map (fun (path, network) -> { Path = path; Network = network })


module Puzzle1 =

  open System.IO

  let steps (doc : Document) =
    let rec loop (label : string) path (steps : int) =
      match path with
      | [] -> loop label doc.Path steps
      | instruction :: continuation ->
        match doc.Network.Next label instruction with
        | "ZZZ" -> steps
        | next -> loop next continuation (steps + 1)
    loop "AAA" doc.Path 1

  let solve (input : string) =
    File.ReadAllText input
    |> getParsed pDocument
    |> steps
  
  

