module AdventOfCode.Day3


open System
open System.IO

type Schematic = (char array) array

[<Struct>]
type Index = { Row : int; Column : int }

[<RequireQualifiedAccess>]
module Index =

  let inline incRow (i : Index) = { i with Row = i.Row + 1 }
  let inline decRow (i : Index) = { i with Row = i.Row - 1 }

  let inline incColumn (i : Index) = { i with Column = i.Column + 1 }
  let inline decColumn (i : Index) = { i with Column = i.Column - 1 }


[<Struct>]
type IndexSpan = { Start : Index; Length : int }

[<RequireQualifiedAccess>]
module IndexSpan =

  let inline startOf (i : IndexSpan) = i.Start

  let inline endOf (i : IndexSpan) = { i.Start with Column = i.Start.Column + i.Length - 1 }

  let adjacents (span : IndexSpan) =
    let result = ResizeArray (2 + 2 * (span.Length + 2))
    let x = Index.decColumn (startOf span)
    let y = Index.incColumn (endOf span)
    result.Add x
    result.Add y
    let u = Index.incRow x
    let v = Index.decRow x
    for i = 0 to span.Length + 1 do
      result.Add { u with Column = x.Column + i }
      result.Add { v with Column = x.Column + i }
    result


[<RequireQualifiedAccess>]
module Char =

  let inline isSymbol c = (c <> '.')
  
  let inline isDigit c = Char.IsDigit c
  
  let inline toInt c = int c - int '0'


let numberSpan (schm : Schematic) (start : Index) =
  let row = schm[start.Row]
  let mutable i = start.Column
  while i < row.Length && Char.isDigit row[i] do
    i <- i + 1
  if i = start.Column then None
  else Some { Start = start; Length = i - start.Column }

let numberSpanToInt (schm : Schematic) (span : IndexSpan) =
  let row = schm[span.Start.Row]
  let mutable n = 0
  for i = 0 to span.Length - 1 do
    n <- n * 10 + Char.toInt row[span.Start.Column + i]
  n

let isSymbol (schm : Schematic) (index : Index) =
  index.Row >= 0 && index.Row < schm.Length &&
  index.Column >= 0 && index.Column < schm.[index.Row].Length &&
  Char.isSymbol schm[index.Row].[index.Column]

let isSymbolAdjacent (schm : Schematic) (span : IndexSpan) =
  span |> IndexSpan.adjacents |> Seq.exists (isSymbol schm)

let accumulateNumberSpans (schm : Schematic) (result : ResizeArray<IndexSpan>) =
  for r = 0 to schm.Length - 1 do
    let mutable c = 0
    while c < schm[r].Length do
      match numberSpan schm { Row = r; Column = c } with
      | Some span  -> result.Add span; c <- span.Start.Column + span.Length
      | None -> c <- c + 1

let getSchematic (input : string) =
  input
  |> File.ReadAllLines
  |> Array.map (fun s -> s.ToCharArray ())

module Puzzle1 =
  let solve (input : string) =
    let schm = getSchematic input
    let numberSpans = ResizeArray<IndexSpan> ()
    accumulateNumberSpans schm numberSpans
    numberSpans
    |> Seq.filter (isSymbolAdjacent schm)
    |> Seq.sumBy (numberSpanToInt schm)

