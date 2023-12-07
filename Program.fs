module AdventOfCode.Program

open AdventOfCode


[<EntryPoint>]
let main _ =

  printfn "Day 1, Puzzle 1: %A" (Day1.Puzzle1.solve "Input/Day1/puzzle.txt")
  printfn "Day 1, Puzzle 2: %A" (Day1.Puzzle2.solve "Input/Day1/puzzle.txt")

  printfn "Day 2, Puzzle 1: %A" (Day2.Puzzle1.solve "Input/Day2/puzzle.txt")
  printfn "Day 2, Puzzle 2: %A" (Day2.Puzzle2.solve "Input/Day2/puzzle.txt")

  0
