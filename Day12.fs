module AdventOfCode.Day12

open System.IO

[<RequireQualifiedAccess>]
module Array =

  let setCopy (arr : 'a []) (i : int) (v : 'a) =
    let a = Array.copy arr
    a[i] <- v; a
  
  let concatWith (sep : 'a) (xss : 'a array seq) =
    let result = Array.zeroCreate (Seq.sumBy ((+) 1 << Array.length) xss - 1)
    xss
    |> Seq.map (Seq.append (Seq.singleton sep))
    |> Seq.concat
    |> Seq.iteri (fun i x -> if i > 0 then result[i - 1] <- x)
    result


type Condition =
  | Operational
  | Broken
  | Unknown

type Pattern = Condition array

type Contiguous = int list

type Cache = Map<Pattern * Contiguous, int64>

let rec compute (cache : Cache) (pattern : Pattern, nums : Contiguous) =
  match Map.tryFind (pattern, nums) cache with
  | Some result -> cache, result
  | None ->
    let cache', result = dispatch cache (pattern, nums)
    Map.add (pattern, nums) result cache', result

and private dispatch cache (pattern, nums) =
  match Array.tryHead pattern with
  | Some Operational -> processOperational cache (pattern, nums)
  | Some Broken -> processBroken cache (pattern, nums)
  | Some Unknown -> processUnknown cache (pattern, nums)
  | None -> cache, if nums = [] then 1L else 0L

and private processOperational cache (pattern, nums) =
  compute cache (pattern[1 ..], nums)

and private processBroken cache (pattern, nums) =
  match nums with
  | [] -> cache, 0L
  | num :: nums' ->
    let notOperational =
      pattern
      |> Seq.takeWhile ((<>) Operational)
      |> Seq.length
    if notOperational < num then cache, 0L
    elif pattern.Length = num then compute cache ([||], nums')
    elif pattern[num] = Broken then cache, 0L
    else compute cache (pattern[num + 1 ..], nums')

and private processUnknown cache (pattern, nums) =
  let pattern1 = Array.setCopy pattern 0 Operational
  let cache', result1 = compute cache (pattern1, nums)
  let pattern2 = Array.setCopy pattern 0 Broken
  let cache'', result2 = compute cache' (pattern2, nums)
  cache'', result1 + result2

let parse (input : string) =
  let parseLine (line : string) =
    let patStr, numStr =
      let i = line.IndexOf " "
      if i < -1 && i >= line.Length then line, ""
      else line.Substring (0, i), line.Substring (i + 1)
    let pattern =
      patStr
      |> Seq.map (fun c ->
        match c with
        | '.' -> Operational
        | '#' -> Broken
        | '?' -> Unknown
        | _ -> failwith $"invalid pattern char {c}")
      |> Seq.toArray
    let nums = (Seq.toList << Seq.map int) (numStr.Split ',')
    pattern, nums
  File.ReadAllLines input
  |> Seq.map parseLine


module Puzzle1 =

  let solve (input : string) =
    parse input
    |> Seq.map (compute Map.empty)
    |> Seq.sumBy snd

module Puzzle2 =

  let unfold times (pattern : Pattern, nums : Contiguous) =
    let pattern' = Array.concatWith Unknown (Seq.replicate times pattern)
    let nums' = List.concat (List.replicate times nums)
    pattern', nums'
  
  let solve (input : string) =
    parse input
    |> Seq.map (unfold 5)
    |> Seq.map (compute Map.empty)
    |> Seq.sumBy snd