module AdventOfCode.Day5

open AdventOfCode.Parsec
open System.IO

[<Struct>]
type Data = { Destination : int64; Source : int64; Range : int64 }

type IntMap = Data list

[<RequireQualifiedAccess>]
module IntMap =

  let private inRange (i : int64) (d : Data) =
    i >= d.Source && i <= d.Source + d.Range

  let find (k : int64) (m : IntMap) =
    List.tryFind (inRange k) m
    |> Option.map (fun d -> d.Destination + k - d.Source)
    |> Option.defaultValue k


type Almanac =
  { Seeds : int64 list
    SeedToSoil : IntMap
    SoilToFertilizer : IntMap
    FertilizerToWater : IntMap
    WaterToLight : IntMap
    LightToTemperature : IntMap
    TemperatureToHumidity : IntMap
    HumidityToLocation : IntMap
  }

let pIntMap =
  pLong >> pLong >> pLong
  |> map (fun ((d, s), r) -> { Destination = d; Source = s; Range = r })
  |> pAtLeastOne

let pAlmanac =
  (pWord "seeds:" >>. pAtLeastOne pLong) >>
  (pWord "seed-to-soil map:" >>. pIntMap) >>
  (pWord "soil-to-fertilizer map:" >>. pIntMap) >>
  (pWord "fertilizer-to-water map:" >>. pIntMap) >>
  (pWord "water-to-light map:" >>. pIntMap) >>
  (pWord "light-to-temperature map:" >>. pIntMap) >>
  (pWord "temperature-to-humidity map:" >>. pIntMap) >>
  (pWord "humidity-to-location map:" >>. pIntMap)
  |> map (fun ((((((( seeds
                    , seedToSoil)
                    , soilToFertilizer)
                    , fertilizerToWater)
                    , waterToLight)
                    , lightToTemperature)
                    , temperatureToHumidity)
                    , humidityToLocation ) ->
    { Seeds = seeds
      SeedToSoil = seedToSoil
      SoilToFertilizer = soilToFertilizer
      FertilizerToWater = fertilizerToWater
      WaterToLight = waterToLight
      LightToTemperature = lightToTemperature
      TemperatureToHumidity = temperatureToHumidity
      HumidityToLocation = humidityToLocation
    })

let getLocation (almanac : Almanac) (seed : int64) =
  let soil = IntMap.find seed almanac.SeedToSoil
  let fertilizer = IntMap.find soil almanac.SoilToFertilizer
  let water = IntMap.find fertilizer almanac.FertilizerToWater
  let light = IntMap.find water almanac.WaterToLight
  let temperature = IntMap.find light almanac.LightToTemperature
  let humidity = IntMap.find temperature almanac.TemperatureToHumidity
  let location = IntMap.find humidity almanac.HumidityToLocation
  location


module Puzzle1 =

  let solve (input : string) =
    let almanac = getParsed pAlmanac (File.ReadAllText input)
    almanac.Seeds |> List.map (getLocation almanac) |> List.min
