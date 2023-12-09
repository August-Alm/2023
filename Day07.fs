module AdventOfCode.Day7

open AdventOfCode.Parsec

[<RequireQualifiedAccess>]
type Card =
  | A = 14
  | K = 13
  | Q = 12
  | J = 11
  | T = 10
  | Nine = 9
  | Eight = 8
  | Seven = 7
  | Six = 6
  | Five = 5
  | Four = 4
  | Three = 3
  | Two = 2

let strengthOfCard (card : Card) =
  match card with
  | Card.A -> 14
  | Card.K -> 13
  | Card.Q -> 12
  | Card.J -> 11
  | Card.T -> 10
  | Card.Nine -> 9
  | Card.Eight -> 8
  | Card.Seven -> 7
  | Card.Six -> 6
  | Card.Five -> 5
  | Card.Four -> 4
  | Card.Three -> 3
  | Card.Two -> 2

type Hand =
  { Card1 : Card
    Card2 : Card
    Card3 : Card
    Card4 : Card
    Card5 : Card
  }

type Type =
  | FiveOfKind
  | FourOfKind
  | FullHouse
  | ThreeOfKind
  | TwoPairs
  | OnePair
  | HighCard

[<RequireQualifiedAccess>]
module Hand =

  let toList (hand : Hand) =
    [ hand.Card1; hand.Card2; hand.Card3; hand.Card4; hand.Card5 ]
  
  let ofList (cards : Card list) =
    match cards with
    | [ c1; c2; c3; c4; c5 ] ->
      { Card1 = c1; Card2 = c2; Card3 = c3; Card4 = c4; Card5 = c5 }
    | _ -> failwith "invalid hand"

  let mkSortedHand (hand : Hand) =
    let add (m : Map<Card, int list>) (card : Card, i : int) =
      match Map.tryFind card m with
      | None -> Map.add card [ i ] m
      | Some is -> Map.add card (i :: is) m
    List.fold add Map.empty
      [ hand.Card1, 1
        hand.Card2, 2
        hand.Card3, 3
        hand.Card4, 4
        hand.Card5, 5
      ]
    |> Map.toList
    
  let typeOf (hand : Hand) =
    match mkSortedHand hand with
    | [ (card, [ 1; 2; 3; 4; 5 ]) ] -> FiveOfKind
    | [ (cardA, idxsA); (cardB, idxsB) ] ->
      match idxsA.Length, idxsB.Length with
      | 4, 1 -> FourOfKind
      | 1, 4 -> FourOfKind
      | 3, 2 -> FullHouse
      | 2, 3 -> FullHouse
      | _ -> failwith "invalid hand"
    | [ (cardA, idxsA); (cardB, idxsB); (cardC, idxsC) ] ->
      match idxsA.Length, idxsB.Length, idxsC.Length with
      | 3, 1, 1 -> ThreeOfKind
      | 1, 3, 1 -> ThreeOfKind
      | 1, 1, 3 -> ThreeOfKind
      | 2, 2, 1 -> TwoPairs
      | 2, 1, 2 -> TwoPairs
      | 1, 2, 2 -> TwoPairs
      | _ -> failwith "invalid hand"
    | [ (cardA, idxsA); (cardB, idxsB); (cardC, idxsC); (cardD, idxsD) ] ->
      match idxsA.Length, idxsB.Length, idxsC.Length, idxsD.Length with
      | 2, 1, 1, 1 -> OnePair
      | 1, 2, 1, 1 -> OnePair
      | 1, 1, 2, 1 -> OnePair
      | 1, 1, 1, 2 -> OnePair
      | _ -> failwith "invalid hand"
    | _ -> HighCard

  let compare (handA : Hand) (handB : Hand) =
    let rec compareCards handA handB =
      let rec loop cs1 cs2 =
        match cs1, cs2 with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | c1 :: cs1, c2 :: cs2 ->
          let cmp = strengthOfCard c1 - strengthOfCard c2
          if cmp <> 0 then cmp else loop cs1 cs2
      loop (toList handA) (toList handB)
    match typeOf handA, typeOf handB with
    | FiveOfKind, FiveOfKind -> compareCards handA handB
    | FiveOfKind, _ -> 1
    | _, FiveOfKind -> -1
    | FourOfKind, FourOfKind -> compareCards handA handB
    | FourOfKind, _ -> 1
    | _, FourOfKind -> -1
    | FullHouse, FullHouse -> compareCards handA handB
    | FullHouse, _ -> 1
    | _, FullHouse -> -1
    | ThreeOfKind, ThreeOfKind -> compareCards handA handB
    | ThreeOfKind, _ -> 1
    | _, ThreeOfKind -> -1
    | TwoPairs, TwoPairs -> compareCards handA handB
    | TwoPairs, _ -> 1
    | _, TwoPairs -> -1
    | OnePair, OnePair -> compareCards handA handB
    | OnePair, _ -> 1
    | _, OnePair -> -1
    | HighCard, HighCard -> compareCards handA handB


let pCard : Parsec<Card> =
  fun line ->
    if line.Length < 1 then None
    else
      match line[0] with
      | 'A' -> Some (Card.A, line.Substring 1)
      | 'K' -> Some (Card.K, line.Substring 1)
      | 'Q' -> Some (Card.Q, line.Substring 1)
      | 'J' -> Some (Card.J, line.Substring 1)
      | 'T' -> Some (Card.T, line.Substring 1)
      | '9' -> Some (Card.Nine, line.Substring 1)
      | '8' -> Some (Card.Eight, line.Substring 1)
      | '7' -> Some (Card.Seven, line.Substring 1)
      | '6' -> Some (Card.Six, line.Substring 1)
      | '5' -> Some (Card.Five, line.Substring 1)
      | '4' -> Some (Card.Four, line.Substring 1)
      | '3' -> Some (Card.Three, line.Substring 1)
      | '2' -> Some (Card.Two, line.Substring 1)
      | _ -> None

let pHand = map Hand.ofList (pAtLeastOne pCard)

let pHandAndBid = pHand >> pInt

module Puzzle1 =

  open System.IO

  let solve (input : string) =
    File.ReadAllLines input
    |> Array.map (getParsed pHandAndBid)
    |> Array.sortWith (fun a b -> Hand.compare (fst a) (fst b))
    |> Array.mapi (fun rank (_, bid) -> (rank + 1) * bid)
    |> Array.sum