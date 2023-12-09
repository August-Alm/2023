module AdventOfCode.Day5

open AdventOfCode.Parsec
open System.IO

type MapData =
  { Destination : int64; Source : int64; Length : int64 }

type IntMap = MapData list

[<RequireQualifiedAccess>]
module IntMap =

  let private inRange (i : int64) (d : MapData) =
    i >= d.Source && i <= d.Source + d.Length

  let find (k : int64) (m : IntMap) =
    List.tryFind (inRange k) m
    |> Option.map (fun d -> d.Destination + k - d.Source)
    |> Option.defaultValue k

type Almanac = { Seeds : int64 list; IntMaps : IntMap list }

let pIntMap =
  pLong >> pLong >> pLong
  |> map (fun ((d, s), r) -> { Destination = d; Source = s; Length = r })
  |> pAtLeastOne

let pIntMaps : Parsec<IntMap list> =
  (pWord "seed-to-soil map:" >>. pIntMap) >>
  (pWord "soil-to-fertilizer map:" >>. pIntMap) >>
  (pWord "fertilizer-to-water map:" >>. pIntMap) >>
  (pWord "water-to-light map:" >>. pIntMap) >>
  (pWord "light-to-temperature map:" >>. pIntMap) >>
  (pWord "temperature-to-humidity map:" >>. pIntMap) >>
  (pWord "humidity-to-location map:" >>. pIntMap)
  |> map (fun (((((( seedToSoil
                   , soilToFertilizer)
                   , fertilizerToWater)
                   , waterToLight)
                   , lightToTemperature)
                   , temperatureToHumidity)
                   , humidityToLocation ) ->
    [ seedToSoil
      soilToFertilizer
      fertilizerToWater
      waterToLight
      lightToTemperature
      temperatureToHumidity
      humidityToLocation
    ])

let pAlmanac : Parsec<Almanac> =
  (pWord "seeds:" >>. pAtLeastOne pLong) >> pIntMaps
  |> map (fun (seeds, intMaps) -> { Seeds = seeds; IntMaps = intMaps })

let getLocation (maps : IntMap list) (seed : int64) =
  List.fold IntMap.find seed maps


module Puzzle1 =

  let solve (input : string) =
    let almanac = getParsed pAlmanac (File.ReadAllText input)
    almanac.Seeds |> List.map (getLocation almanac.IntMaps) |> List.min


module Puzzle2 =

  open System.Collections.Generic

  type Range = { Start : int64; Length : int64 }
  with member this.End = this.Start + this.Length - 1L

  type Almanac2 = { Seeds : Range list; IntMaps : IntMap list }

  let pRange =
    pLong >> pLong
    |> map (fun (s, l) -> { Start = s; Length = l })

  let pSeeds =
    pWord "seeds:" >>. pAtLeastOne pRange

  let pAlmanac2 =
    pSeeds >> pIntMaps
    |> map (fun (seeds, intMaps) -> { Seeds = seeds; IntMaps = intMaps })

  let mkRangeMap (m : IntMap) =
    List.fold (fun m d ->
      let inRange = { Start = d.Source; Length = d.Length }
      let outRange = { Start = d.Destination; Length = d.Length }
      Map.add inRange outRange m)
      Map.empty m
  
  let transform (ranges : Range seq) (rangeMap : Map<Range, Range>) =
    let inline mkRange s e =
      { Start = s; Length = e - s + 1L }
    let inline intersects (r1 : Range) (r2 : Range) =
      r1.Start <= r2.End && r2.Start <= r1.End
    let input = Queue<Range> ranges
    let output = ResizeArray<Range> ()
    let mutable range = Unchecked.defaultof<Range>
    while input.TryDequeue &range do
      match rangeMap.Keys |> Seq.tryFind (intersects range) with
      | None -> output.Add range
      | Some source ->
        if source.Start <= range.Start && range.End <= source.End then
          let destination = rangeMap[source]
          let shift = destination.Start - source.Start
          output.Add (mkRange (range.Start + shift) (range.End + shift))
        elif range.Start < source.Start then
          input.Enqueue (mkRange range.Start (source.Start - 1L))
          input.Enqueue (mkRange source.Start range.End)
        else
          input.Enqueue (mkRange range.Start source.End)
          input.Enqueue (mkRange (source.End + 1L) range.End)
    output :> seq<_>

  let solve (input : string) =
    let almanac = getParsed pAlmanac2 (File.ReadAllText input)
    almanac.IntMaps
    |> List.map mkRangeMap
    |> Seq.fold transform almanac.Seeds
    |> Seq.map _.Start
    |> Seq.min