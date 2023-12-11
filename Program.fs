﻿module AdventOfCode.Program

open AdventOfCode
open System.IO

  
[<EntryPoint>]
let main _ =

  let input i = Path.Combine ("Input", sprintf "Day%02i" i, $"puzzle.txt")

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
  printfn "Day 9, Puzzle 1: %A" (Day9.Puzzle2.solve (input 9))

  0
