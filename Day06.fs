module AdventOfCode.Day6

open AdventOfCode.Parsec
open LanguagePrimitives
open System
open System.IO

[<Measure>] type ms

[<Measure>] type mm

type Time = int<ms>

type Distance = int<mm>

type Race = { Time : Time; Distance : Distance }

type Sheet = Race list

let pSheet =
  let pTimes = pWord "Time:" >>. pAtLeastOne pInt
  let pDistances = pWord "Distance:" >>. pAtLeastOne pInt
  pTimes >> pDistances
  |> map (fun (ts, ds) ->
    List.zip ts ds |> List.map (fun (t, d) ->
      { Time = Int32WithMeasure t; Distance = Int32WithMeasure d }))

let speed (hold : Time) = hold * 1<mm/ms^2>

let distance (race : Race) (hold : Time) =
  if hold < 0<ms> || hold > race.Time then failwith "Invalid hold time"
  else (race.Time - hold) * speed hold

// The `distance` function is a quadratic polynomial in `hold`. The solutions
// to the equation `distance race hold = race.Distance` are
// `hold = (race.Time / 2) ± Math.Sqrt ((race.Time /2)^2 - race.Distance)`.
// From this we can immediately calculate the number of `hold` values that
// result in a distance of at least `race.Distance`:
let numberOfWinningWays (race : Race) =
  let timeOverTwo = 0.5 * double (race.Time / 1<ms>)
  let delta = timeOverTwo * timeOverTwo - double race.Distance
  if delta < 0.0 then
    0
  else
    let discriminant = Math.Sqrt delta
    let hold1 = timeOverTwo - discriminant
    let hold2 = timeOverTwo + discriminant
    1.0 + Math.Floor hold2 - Math.Ceiling hold1
    |> int

module Puzzle1 =
  let solve (input : string) =
    File.ReadAllText input
    |> getParsed pSheet
    |> List.map numberOfWinningWays
    |> List.reduce (*)

