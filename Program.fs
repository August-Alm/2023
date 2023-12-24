module AdventOfCode.Program

open AdventOfCode
open System.IO

  
[<EntryPoint>]
let main _ =

  let input i = Path.Combine ("Input", sprintf "Day%02i" i, $"puzzle.txt")
  let test i = Path.Combine ("Input", sprintf "Day%02i" i, $"test.txt")

  printfn "Day 1, Puzzle 1: %A" (Day1.Puzzle1.solve (input 1))
  printfn "Day 1, Puzzle 2: %A" (Day1.Puzzle2.solve (input 1))

  printfn "Day 2, Puzzle 1: %A" (Day2.Puzzle1.solve (input 2))
  printfn "Day 2, Puzzle 2: %A" (Day2.Puzzle2.solve (input 2))

  printfn "Day 3, Puzzle 1: %A" (Day3.Puzzle1.solve (input 3))
  printfn "Day 3, Puzzle 2: %A" (Day3.Puzzle2.solve (input 3))

  printfn "Day 4, Puzzle 1: %A" (Day4.Puzzle1.solve (input 4))
  printfn "Day 4, Puzzle 1: %A" (Day4.Puzzle2.solve (input 4))

  printfn "Day 5, Puzzle 1: %A" (Day5.Puzzle1.solve (input 5))
  printfn "Day 5, Puzzle 2: %A" (Day5.Puzzle2.solve (input 5))

  printfn "Day 6, Puzzle 1: %A" (Day6.Puzzle1.solve (input 6))
  printfn "Day 6, Puzzle 2: %A" (Day6.Puzzle2.solve (input 6))

  printfn "Day 7, Puzzle 1: %A" (Day7.Puzzle1.solve (input 7))
  printfn "Day 7, Puzzle 2: %A" (Day7.Puzzle2.solve (input 7))

  printfn "Day 8, Puzzle 1: %A" (Day8.Puzzle1.solve (input 8))
  printfn "Day 8, Puzzle 2: %A" (Day8.Puzzle2.solve (input 8))

  printfn "Day 9, Puzzle 1: %A" (Day9.Puzzle1.solve (input 9))
  printfn "Day 9, Puzzle 2: %A" (Day9.Puzzle2.solve (input 9))

  printfn "Day 10, Puzzle 1: %A" (Day10.Puzzle1.solve (input 10))
  printfn "Day 10, Puzzle 2: %A" (Day10.Puzzle2.solve (input 10))

  printfn "Day 11, Puzzle 1: %A" (Day11.Puzzle1.solve (input 11))
  printfn "Day 11, Puzzle 2: %A" (Day11.Puzzle2.solve (input 11))

  printfn "Day 12, Puzzle 1: %A" (Day12.Puzzle1.solve (input 12))
  printfn "Day 12, Puzzle 2: %A" (Day12.Puzzle2.solve (input 12))

  printfn "Day 13, Puzzle 1: %A" (Day13.Puzzle1.solve (input 13))
  printfn "Day 13, Puzzle 2: %A" (Day13.Puzzle2.solve (input 13))

  printfn "Day 14, Puzzle 1: %A" (Day14.Puzzle1.solve (input 14))
  printfn "Day 14, Puzzle 2: %A" (Day14.Puzzle2.solve (input 14))

  printfn "Day 15, Puzzle 1: %A" (Day15.Puzzle1.solve (input 15))
  printfn "Day 15, Puzzle 2: %A" (Day15.Puzzle2.solve (input 15))

  printfn "Day 16, Puzzle 1: %A" (Day16.Puzzle1.solve (input 16))
  //printfn "Day 16, Puzzle 2: %A" (Day16.Puzzle2.solve (input 16))

  printfn "Day 17, Puzzle 1: %A" (Day17.Puzzle1.solve (input 17))
  printfn "Day 17, Puzzle 2: %A" (Day17.Puzzle2.solve (input 17))

  printfn "Day 18, Puzzle 1: %A" (Day18.Puzzle1.solve (input 18))
  printfn "Day 18, Puzzle 2: %A" (Day18.Puzzle2.solve (input 18))

  printfn "Day 19, Puzzle 1: %A" (Day19.Puzzle1.solve (input 19))
  printfn "Day 19, Puzzle 2: %A" (Day19.Puzzle2.solve (input 19))

  printfn "Day 20, Puzzle 1: %A" (Day20.Puzzle1.solve (input 20))
  printfn "Day 20, Puzzle 2: %A" (Day20.Puzzle2.solve (input 20))

  printfn "Day 21, Puzzle 1: %A" (Day21.Puzzle1.solve (input 21))
  //printfn "Day 21, Puzzle 2: %A" (Day21.Puzzle2.solve (input 21))

  printfn "Day 22, Puzzle 1: %A" (Day22.Puzzle1.solve (input 22))
  printfn "Day 22, Puzzle 2: %A" (Day22.Puzzle2.solve (input 22))

  0
