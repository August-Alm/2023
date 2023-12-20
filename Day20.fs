module AdventOfCode.Day20

[<Struct>]
type Pulse = Low | High

type Signal =
  { Sender: string
    Receiver: string
    Pulse: Pulse
  }

type Module =
  { Inputs : string list
    Process : Signal -> (Signal seq)
    Reset : unit -> unit
  }

[<RequireQualifiedAccess>]
module Module =

  open System.Collections.Generic
  
  let conjunction (name : string) (inputs : string list) (outputs : string list) =
    let recents = Dictionary (Seq.map (fun n -> KeyValuePair (n, Low)) inputs)
    { Inputs = inputs
      Process = fun signal ->
        recents[signal.Sender] <- signal.Pulse
        let pulse = if recents.Values |> Seq.forall ((=) High) then Low else High
        outputs |> Seq.map (fun o -> { Sender = name; Receiver = o; Pulse = pulse })
      Reset = fun () -> for k in recents.Keys do recents[k] <- Low
    }

  let flipflop (name : string) (inputs : string list) (outputs : string list) =
    let mutable state = Low
    { Inputs = inputs
      Process = fun signal ->
        if signal.Pulse = Low then
          if state = Low then state <- High else state <- Low
          outputs |> Seq.map (fun o -> { Sender = name; Receiver = o; Pulse = state })
        else
          Seq.empty
      Reset = fun () -> state <- Low
    }
  
  let repeater (name : string) (inputs : string list) (outputs : string list) =
    { Inputs = inputs
      Process = fun signal ->
        outputs |> Seq.map (fun o -> { Sender = name; Receiver = o; Pulse = signal.Pulse })
      Reset = fun () -> ()
    }

type Wiring = Map<string, Module>

[<RequireQualifiedAccess>]
module Wiring =

  open System
  open System.Text.RegularExpressions
  open System.Collections.Generic

  let parse (lines : string seq) : Wiring =
    let descriptions = ResizeArray ()
    for line in lines do
      let kind = if Char.IsLetter line[0] then "" else string line[0]
      let parts = Regex.Matches (line, "[a-z]+") |> Seq.map _.Value
      descriptions.Add (kind, Seq.head parts, Seq.tail parts |> Seq.toList)
    descriptions.Add ("", "button", ["broadcaster"])
    descriptions.Add ("", "rx", [])
    Seq.fold
      (fun (wiring : Wiring) (kind, name, outputs) ->
        let inputs =
          descriptions
          |> Seq.choose (fun (_, n, outs) ->
            if List.exists ((=) name) outs then Some n else None)
          |> Seq.toList
        let modul =
          match kind with
          | "&" -> Module.conjunction name inputs outputs
          | "%" -> Module.flipflop name inputs outputs
          | _ -> Module.repeater name inputs outputs
        wiring.Add (name, modul)
      )
      Map.empty descriptions
  
  let reset (wiring : Wiring) =
    for modul in wiring.Values do modul.Reset ()
  
  let pushTheButton (wiring : Wiring) =
    let queue = Queue ()
    let mutable signal = { Sender = "button"; Receiver = "broadcaster"; Pulse = Low }
    queue.Enqueue signal
    seq {
      while queue.TryDequeue &signal do
        yield signal
        let receiver = wiring[signal.Receiver]
        Seq.iter queue.Enqueue (receiver.Process signal)
    }
  
  let loopLength (wiring : Wiring) (output : string) =
    let rec foo n =
      let signals = pushTheButton wiring
      if Seq.exists (fun s -> s.Sender = output && s.Pulse = High) signals then n
      else foo (n + 1)
    let r = foo 1
    reset wiring; r

    
module Puzzle1 =
  
    open System.IO

    let solve (input : string) =
      let wiring = Wiring.parse (File.ReadAllLines input)
      let mutable lo, hi = 0, 0
      for _ = 1 to 1000 do
        for signal in Wiring.pushTheButton wiring do
          match signal.Pulse with
          | Low -> lo <- lo + 1
          | High -> hi <- hi + 1
      lo * hi

module Puzzle2 =
  
    open System.IO

    [<RequireQualifiedAccess>]
    module List =
      
      let private lcmOfTwo (a : bigint) (b : int) =
        (a * bigint b) / System.Numerics.BigInteger.GreatestCommonDivisor (a, b)
      
      let lcm (xs : int list) = List.fold lcmOfTwo 1I xs

    let solve (input : string) =
      let wiring = Wiring.parse (File.ReadAllLines input)
      let nand = List.exactlyOne wiring["rx"].Inputs 
      wiring[nand].Inputs
      |> List.map (Wiring.loopLength wiring)
      |> List.lcm
