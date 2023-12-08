module AdventOfCode.Day4

open System.IO
open AdventOfCode.Parsec

type Card =
  { Index : int; Winning : int list; Have : int list }

let pIndex = pWord "Card" >>. pInt .>> pWord ":"

let pWinning = pAtLeastOne pInt

let pHave = pWord "|" >>. pAtLeastOne pInt

let pCard =
  (pIndex >> pWinning >> pHave)
  |> map (fun ((i, w), h) -> { Index = i; Winning = w; Have = h })

module Puzzle1 =

  let score (card : Card) =
    let isWinning x = card.Winning |> List.contains x
    let n = card.Have |> List.sumBy (fun x -> if isWinning x then 1 else 0)
    if n = 0 then 0 else pown 2 (n - 1)

  let solve (input : string) =
    File.ReadAllLines input
    |> Seq.sumBy (score << getParsed pCard)

module Puzzle2 =

  type ParentMap = Map<int, int list>

  [<RequireQualifiedAccess>]
  module ParentMap =

    let add (child : int) (parent : int) (m : ParentMap) =
      match Map.tryFind child m with
      | Some ps -> Map.add child (parent :: ps) m
      | None -> Map.add child [ parent ] m
    
    let rec numberOfAncestors (m : ParentMap) c =
      Map.tryFind c m
      |> Option.map (List.sumBy (fun c -> 1 + numberOfAncestors m c))
      |> Option.defaultValue 0

  let winning (card : Card) =
    let inline isWinning x = card.Winning |> List.contains x
    card.Have |> List.sumBy (fun x -> if isWinning x then 1 else 0)
  
  let mkParentMap (cards : Card seq) =
    let addNext (card : Card) m i =
      ParentMap.add (card.Index + i) card.Index m
    let addParents (m : ParentMap) (card : Card) =
      let w = winning card
      if w = 0 then m
      else List.fold (addNext card)  m [ 1 .. w ]
    Seq.fold addParents Map.empty cards
  
  let solve (input : string) =
    let cards = Seq.map (getParsed pCard) (File.ReadAllLines input)
    let coverMap = mkParentMap cards
    let instances card = 1 + ParentMap.numberOfAncestors coverMap card.Index
    cards |> Seq.sumBy instances