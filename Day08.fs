module AdventOfCode.Day8

open AdventOfCode.Parsec

type Instruction = Left | Right

type Node = string

type Network = Map<Node, Node * Node>

let move (instruction : Instruction) (left : Node, right : Node) =
  match instruction with
  | Left -> left
  | Right -> right

let steps (instructions : Instruction list, network : Network) (start : Node) =
  let rec loop steps instrs node =
    match instrs with
    | [] -> loop steps instructions node
    | instr :: instrs ->
      let next = move instr network[node]
      if next[next.Length - 1] = 'Z' then steps
      else loop (steps + 1) instrs next
  loop 1 instructions start

let pInstruction =
  fun input ->
    match pAnyChar input with
    | Some ('L', inp) -> Some (Left, inp)
    | Some ('R', inp) -> Some (Right, inp)
    | _ -> None

let pNodeDescription =
  let pLabel = pLetters .>> pChar '='
  let pLeft = pChar '(' >>. pLetters
  let pRight = pChar ',' >>. pLetters .>> pChar ')'
  pLabel .>>. (pLeft .>>. pRight)

let pNetwork =
  map (Map.ofList) (pAtLeastOne pNodeDescription)


module Puzzle1 =

  open System.IO

  let solve (input : string) =
    File.ReadAllText input
    |> getParsed (pAtLeastOne pInstruction .>>. pNetwork)
    |> (fun doc -> steps doc "AAA")

module Puzzle2 =

  open System.IO
  open System.Numerics
  
  let lcm (a : bigint) (b : bigint) =
    (a * b) / BigInteger.GreatestCommonDivisor (a, b)

  let ghostSteps (instructions : Instruction list, network : Network) =
    let startNodes = network.Keys |> Seq.filter (fun l -> l[l.Length - 1] = 'A')
    Seq.reduce lcm (Seq.map (steps (instructions, network) >> bigint) startNodes)

  let solve (input : string) =
    File.ReadAllText input
    |> getParsed (pAtLeastOne pInstruction .>>. pNetwork)
    |> ghostSteps