module AdventOfCode.Day2

open AdventOfCode.Parser
open System.IO

module Puzzle1 =

  type Color = Red | Green | Blue

  let pRed = map (fun _ -> Red) (pWord "red")

  let pGreen = map (fun _ -> Green) (pWord "green")

  let pBlue = map (fun _ -> Blue) (pWord "blue")

  let pColor = pRed <|> pGreen <|> pBlue

  [<Struct>]
  type CubeSet =
    { Reds : int
      Greens : int
      Blues : int
    }
  
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

  type Game =
    { Number : int
      CubeSets : CubeSet list
    }
  
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

