module AdventOfCode.Day7

open AdventOfCode.Parsec

[<RequireQualifiedAccess>]
type Card =
  | A
  | K
  | Q
  | J
  | T
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two

[<RequireQualifiedAccess>]
module Card =

  let ofChar (c : char) =
    match c with
    | 'A' -> Card.A
    | 'K' -> Card.K
    | 'Q' -> Card.Q
    | 'J' -> Card.J
    | 'T' -> Card.T
    | '9' -> Card.Nine
    | '8' -> Card.Eight
    | '7' -> Card.Seven
    | '6' -> Card.Six
    | '5' -> Card.Five
    | '4' -> Card.Four
    | '3' -> Card.Three
    | '2' -> Card.Two
    | _ -> failwith $"invalid card: {c}"

  let strength (card : Card) =
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
  
  let jokerStrength (card : Card) =
    if card = Card.J then 1 else strength card


type Hand = { Cards : Card list; Bid : int64 }

type Type =
  | FiveOfKind
  | FourOfKind
  | FullHouse
  | ThreeOfKind
  | TwoPairs
  | OnePair
  | HighCard

[<RequireQualifiedAccess>]
module Type =

  let compare (typ1 : Type) (typ2 : Type) =
    match typ1, typ2 with
    | FiveOfKind, FiveOfKind -> 0
    | FiveOfKind, _ -> 1
    | _, FiveOfKind -> -1
    | FourOfKind, FourOfKind -> 0
    | FourOfKind, _ -> 1
    | _, FourOfKind -> -1
    | FullHouse, FullHouse -> 0
    | FullHouse, _ -> 1
    | _, FullHouse -> -1
    | ThreeOfKind, ThreeOfKind -> 0
    | ThreeOfKind, _ -> 1
    | _, ThreeOfKind -> -1
    | TwoPairs, TwoPairs -> 0
    | TwoPairs, _ -> 1
    | _, TwoPairs -> -1
    | OnePair, OnePair -> 0
    | OnePair, _ -> 1
    | _, OnePair -> -1
    | HighCard, HighCard -> 0
    
[<RequireQualifiedAccess>]
module Hand =

  let typeOf (cards : Card list) =
    cards
    |> List.groupBy Card.strength
    |> List.map (snd >> List.length)
    |> List.sort
    |> function
      | [ 1; 1; 1; 1; 1 ] -> HighCard
      | [ 1; 1; 1; 2 ] -> OnePair
      | [ 1; 2; 2 ] -> TwoPairs
      | [ 1; 1; 3 ] -> ThreeOfKind
      | [ 2; 3 ] -> FullHouse
      | [ 1; 4 ] -> FourOfKind
      | [ 5 ] -> FiveOfKind
      | _ -> failwith $"Invalid hand: {cards}"
  
  let jokerTypeOf (cards : Card list) =
    cards
    |> List.filter ((<>) Card.J)
    |> List.groupBy Card.strength
    |> List.map (snd >> List.length)
    |> List.sort
    |> function
      // Five jokers
      | [] -> FiveOfKind
      // Four jokers
      | [ 1 ] -> FiveOfKind
      // Three jokers
      | [ 1; 1 ] -> FourOfKind
      | [ 2 ] -> FiveOfKind
      // Two jokers
      | [ 1; 1; 1 ] -> ThreeOfKind
      | [ 1; 2 ] -> FourOfKind
      | [ 3 ] -> FiveOfKind
      // One joker
      | [ 1; 1; 1; 1 ] -> OnePair
      | [ 1; 1; 2 ] -> ThreeOfKind
      | [ 1; 3 ] -> FourOfKind
      | [ 2; 2 ] -> FullHouse 
      | [ 4 ] -> FiveOfKind
      // No jokers
      | [ 1; 1; 1; 1; 1 ] -> HighCard
      | [ 1; 1; 1; 2 ] -> OnePair
      | [ 1; 2; 2 ] -> TwoPairs
      | [ 1; 1; 3 ] -> ThreeOfKind
      | [ 2; 3 ] -> FullHouse
      | [ 1; 4 ] -> FourOfKind
      | [ 5 ] -> FiveOfKind
      | _ -> failwith $"Invalid hand: {cards}"
  
  let rec compareCardsBy strength (cs1 : Card list) (cs2 : Card list) =
    match cs1, cs2 with
    | [], [] -> 0
    | [], _ | _, [] -> failwith "all hands must have 5 cards"
    | c1 :: cs1, c2 :: cs2 ->
      match compare (strength c1) (strength c2) with
      | 0 -> compareCardsBy strength cs1 cs2
      | cmp -> cmp
  
  let compareCards = compareCardsBy Card.strength

  let jokerCompareCards = compareCardsBy Card.jokerStrength

  let compare (hand1 : Hand) (hand2 : Hand) =
    match Type.compare (typeOf hand1.Cards) (typeOf hand2.Cards) with
    | 0 -> compareCards hand1.Cards hand2.Cards
    | cmp -> cmp
  
  let jokerCompare (hand1 : Hand) (hand2 : Hand) =
    match Type.compare (jokerTypeOf hand1.Cards) (jokerTypeOf hand2.Cards) with
    | 0 -> jokerCompareCards hand1.Cards hand2.Cards
    | cmp -> cmp


let pHand =
  let pCard = map Card.ofChar pChar
  (pAtLeastOne pCard) .>>. pLong
  |> map (fun (cs, bid) -> { Cards = cs; Bid = bid })


module Puzzle1 =

  open System.IO

  let solve (input : string) =
    File.ReadAllLines input
    |> Array.map (getParsed pHand)
    |> Array.sortWith Hand.compare
    |> Array.mapi (fun rank hand -> (int64 rank + 1L) * hand.Bid)
    |> Array.sum

module Puzzle2 =

  open System.IO

  let solve (input : string) =
    File.ReadAllLines input
    |> Array.map (getParsed pHand)
    |> Array.sortWith Hand.jokerCompare
    |> Array.mapi (fun rank hand -> (int64 rank + 1L) * hand.Bid)
    |> Array.sum