module AdventOfCode.Day2

open System.IO
open System

type Parser<'a> =
  string -> ('a * string) option

let bind (p : Parser<'a>) (f : 'a -> Parser<'b>) : Parser<'b> =
  fun input ->
    p input |> Option.bind (fun (a, inp) -> (f a) inp)

let retur (a : 'a) : Parser<'a> =
  fun input -> Some (a, input)

let (>>=) = bind

let (>>) p1 p2 =
  p1 >>= fun a -> p2 >>= fun b -> retur (a, b)

let (>>.) p1 p2 =
  p1 >>= fun _ -> p2

let (.>>) p1 p2 =
  p1 >>= fun a -> p2 >>= fun _ -> retur a

let (<|>) (p1 : Parser<'a>) (p2 : Parser<'a>) : Parser<'a> =
  fun input ->
    match p1 input with
    | Some _ as r -> r
    | None -> p2 input

let map (f : 'a -> 'b) (p : Parser<'a>) : Parser<'b> =
  fun input ->
    p input |> Option.map (fun (a, inp) -> (f a, inp))

let pWord (w : string) : Parser<string> =
  fun input ->
    let input = input.TrimStart ()
    if input.StartsWith w then Some (w, input.Substring w.Length)
    else None

let pInt : Parser<int> =
  fun input ->
    let input = input.TrimStart ()
    let mutable n = 0
    let mutable i = 0
    while i < input.Length && Char.IsDigit input[i] do
      n <- n * 10 + (int input[i] - int '0')
      i <- i + 1
    if i = 0 then None
    else Some (n, input.Substring i)

let pAtLeastOne sep p =
  let consWith pRem x = map (fun xs -> x :: xs) pRem <|> retur [ x ]
  let rec pTail input = ((pWord sep >>. p) >>= consWith pTail) input
  p >>= consWith pTail


module Puzzle1 =

  type Color = Red | Green | Blue

  let pRed = map (fun _ -> Red) (pWord "red")

  let pGreen = map (fun _ -> Green) (pWord "green")

  let pBlue = map (fun _ -> Blue) (pWord "blue")

  let pColor = pRed <|> pGreen <|> pBlue

  [<Struct>]
  type CubeSet = { Reds : int; Greens : int; Blues : int }
  
  let mkCubeSet (cubes : (int * Color) list) =
    let mutable reds = 0
    let mutable greens = 0
    let mutable blues = 0
    for (n, c) in cubes do
      match c with
      | Red -> reds <- n
      | Green -> greens <- n
      | Blue -> blues <- n
    { Reds = reds; Greens = greens; Blues = blues }

  let pCubeSet = map mkCubeSet (pAtLeastOne "," (pInt >> pColor))

  let pCubeSets = pAtLeastOne ";" pCubeSet

  type Game = { Number : int; CubeSets : CubeSet list }
  
  let pGame =
    (pWord "Game" >>. pInt .>> pWord ":") >> pCubeSets
    |> map (fun (n, r) -> { Number = n; CubeSets = r })

  let isPossible (s : CubeSet) =
    s.Reds <= 12 && s.Greens <= 13 && s.Blues <= 14

  let getGame (line : string) =
    match pGame line with
    | Some (g, _) -> Some g
    | _ -> None

  let solve (input : string) =
    File.ReadAllLines input
    |> Seq.choose getGame
    |> Seq.filter (List.forall isPossible << _.CubeSets)
    |> Seq.sumBy _.Number


module Puzzle2 =

  open Puzzle1

  let minimum (sets : CubeSet list) =
    let mutable reds = 0
    let mutable greens = 0
    let mutable blues = 0
    for s in sets do
      if s.Reds > reds then reds <- s.Reds
      if s.Greens > greens then greens <- s.Greens
      if s.Blues > blues then blues <- s.Blues
    { Reds = reds; Greens = greens; Blues = blues }
  
  let power (set : CubeSet) =
    set.Reds * set.Greens * set.Blues
  
  let solve (input : string) =
    File.ReadAllLines input
    |> Seq.choose getGame
    |> Seq.sumBy (power << minimum << _.CubeSets)

