module AdventOfCode.Day19

open System

[<RequireQualifiedAccess>]
module String =

  let blocks (s : string) =
    s.Split ([| "\r\n\r\n"; "\n\n" |], StringSplitOptions.RemoveEmptyEntries)
  
  let lines (s : string) =
    s.Split ([| "\r\n"; "\n" |], StringSplitOptions.RemoveEmptyEntries)


[<Struct; RequireQualifiedAccess>]
type Category =
  | X
  | M
  | A
  | S

[<Struct; RequireQualifiedAccess>]
type Rating =
  { X : int
    M : int
    A : int
    S : int
  }

[<RequireQualifiedAccess>]
module Rating =

  let parse (line : string) =
    let content = line.Substring (1, line.Length - 2)
    let assignments = content.Split ","
    let x = Int32.Parse (assignments[0].AsSpan().Slice 2)
    let m = Int32.Parse (assignments[1].AsSpan().Slice 2)
    let a = Int32.Parse (assignments[2].AsSpan().Slice 2)
    let s = Int32.Parse (assignments[3].AsSpan().Slice 2)
    { Rating.X = x; Rating.M = m; Rating.A = a; Rating.S = s }

  let get category (rating : Rating) =
    match category with
    | Category.X -> rating.X
    | Category.M -> rating.M
    | Category.A -> rating.A
    | Category.S -> rating.S

[<Struct>]
type Verdict =
  | Approved
  | Rejected
  | Pending of string

[<RequireQualifiedAccess>]
module Verdict =

  let parse (item : string) =
    match item with
    | "A" -> Approved
    | "R" -> Rejected
    | _ -> Pending item


type Condition =
  | LessThan of (Category * int)
  | GreaterThan of (Category * int)
  | CatchAll

[<RequireQualifiedAccess>]
module Condition =

  let apply (condition : Condition) (rating : Rating) =
    match condition with
    | LessThan (category, value) -> Rating.get category rating < value
    | GreaterThan (category, value) -> Rating.get category rating > value
    | CatchAll -> true


[<RequireQualifiedAccess>]
type Rule =
  { Condition : Condition
    Destination : Verdict
  }

[<RequireQualifiedAccess>]
module Rule =

  let parse (item : string) : Rule =
    let i = item.IndexOf ':'
    if i < 0 then
      let v = Verdict.parse item
      { Condition = CatchAll; Destination = v }
    else
      let parts = item.Split ':'
      let dest = Verdict.parse (parts[1])
      let span = parts[0].AsSpan ()
      let value = Int32.Parse (span.Slice 2)
      let category =
        match span[0] with
        | 'x' -> Category.X
        | 'm' -> Category.M
        | 'a' -> Category.A
        | 's' -> Category.S
        | c -> failwithf "invalid category: %c" c
      let condition =
        match span[1] with
        | '<' -> LessThan (category, value)
        | '>' -> GreaterThan (category, value)
        | c -> failwithf "unsupported comparison operator: %c" c
      { Condition = condition; Destination = dest }
  
  let rec apply (rules : Rule list) (rating : Rating) =
    match rules with
    | [] -> failwith "no rule to apply"
    | rule :: _ when Condition.apply rule.Condition rating -> rule.Destination
    | _ :: rest -> apply rest rating


type Workflow = { Name : string; Rules : Rule list }

type Workflows = Map<string, Workflow>

[<RequireQualifiedAccess>]
module Workflow =

  let private parseLine (line : string) =
    let i = line.IndexOf '{'
    let name = line.Substring (0, i)
    let rulesStr = line.Substring (i + 1, line.Length - i - 2)
    let rules = rulesStr.Split "," |> Seq.map Rule.parse |> Seq.toList
    { Name = name; Rules = rules }
  
  let parse (lines : string array) =
    Map.ofSeq (Seq.map (parseLine >> fun wf -> wf.Name, wf) lines)
  
  let inWorkflow (workflows : Workflows) =
    Map.find "in" workflows
  
  let verdict (workflows : Workflows) (rating : Rating) =
    let rec foo workflow rating =
      match Rule.apply workflow.Rules rating with
      | Pending name -> foo (Map.find name workflows) rating
      | Approved -> Approved
      | Rejected -> Rejected
    foo (inWorkflow workflows) rating


module Puzzle1 =

  open System.IO

  let solve (input : string) =
    let blocks = String.blocks (File.ReadAllText input)
    let workflows = Workflow.parse (String.lines blocks[0])
    String.lines blocks[1]
    |> Seq.map Rating.parse
    |> Seq.filter (Workflow.verdict workflows >> ((=) Approved))
    |> Seq.sumBy (fun r -> r.X + r.M + r.A + r.S)
    

module Puzzle2 =

  open System.IO

  [<Struct>]
  type Range = { Min : int; Max : int }

  [<RequireQualifiedAccess>]
  module Range =

    let splitAt m (range : Range) =
      { range with Max = min (m - 1) range.Max },
      { range with Min = max m range.Min }

  type Cube =
    { RangeX : Range; RangeM : Range; RangeA : Range; RangeS : Range }

  [<RequireQualifiedAccess>]
  module Cube =

    let isDegenerate (cube : Cube) =
      cube.RangeX.Min > cube.RangeX.Max ||
      cube.RangeM.Min > cube.RangeM.Max ||
      cube.RangeA.Min > cube.RangeA.Max ||
      cube.RangeS.Min > cube.RangeS.Max
    
    let volume (cube : Cube) =
      let x = cube.RangeX.Max - cube.RangeX.Min + 1 |> bigint
      let m = cube.RangeM.Max - cube.RangeM.Min + 1 |> bigint
      let a = cube.RangeA.Max - cube.RangeA.Min + 1 |> bigint
      let s = cube.RangeS.Max - cube.RangeS.Min + 1 |> bigint
      x * m * a * s
    
    let setRange (category : Category) (range : Range) (cube : Cube) =
      match category with
      | Category.X -> { cube with RangeX = range }
      | Category.M -> { cube with RangeM = range }
      | Category.A -> { cube with RangeA = range }
      | Category.S -> { cube with RangeS = range }
    
    let getRange (category : Category) (cube : Cube) =
      match category with
      | Category.X -> cube.RangeX
      | Category.M -> cube.RangeM
      | Category.A -> cube.RangeA
      | Category.S -> cube.RangeS
    

  let rec foo (verdict : Verdict) (workflows : Workflows) (cube : Cube) =
    match verdict with
    | Approved -> Cube.volume cube
    | Rejected -> 0I
    | Pending name -> bar workflows (Map.find name workflows) cube
        
  and bar workflows workflow cube =
    List.fold
      (fun (acc, cube) (rule : Rule) ->
        match rule.Condition with
        | CatchAll -> acc + foo rule.Destination workflows cube, cube
        | LessThan (cat, value) ->
          let lo, hi = Range.splitAt value (Cube.getRange cat cube)
          acc + foo rule.Destination workflows (Cube.setRange cat lo cube),
          Cube.setRange cat hi cube
        | GreaterThan (cat, value) ->
          let lo, hi = Range.splitAt (value + 1) (Cube.getRange cat cube)
          acc + foo rule.Destination workflows (Cube.setRange cat hi cube),
          Cube.setRange cat lo cube
      )
      (0I, cube) workflow.Rules
    |> fst
  
  let acceptedVolume (workflows : Workflows) cube =
    bar workflows (Workflow.inWorkflow workflows) cube

  let theCube =
    { RangeX = { Min = 1; Max = 4000 }
      RangeM = { Min = 1; Max = 4000 }
      RangeA = { Min = 1; Max = 4000 }
      RangeS = { Min = 1; Max = 4000 }
    }
  
  let solve (input : string) =
    let blocks = String.blocks (File.ReadAllText input)
    let workflows = Workflow.parse (String.lines blocks[0])
    acceptedVolume workflows theCube