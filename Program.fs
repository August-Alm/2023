module AdventOfCode.Program

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

  0
