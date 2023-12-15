module AdventOfCode.Day15


[<RequireQualifiedAccess>]
module Hash =

  let calculate (s : string) =
    Seq.fold (fun current c -> (current + uint8 c) * 17uy) 0uy s
    |> int


module Puzzle1 =

  open System.IO

  let solve (input : string) =
    ((File.ReadAllLines input)[0]).Split ',' 
    |> Array.sumBy Hash.calculate


type Lens = { Label : string; FocalLength : int }

type Step = { Label : string; FocalLength : int option }

[<RequireQualifiedAccess>]
module Step =
  
  let parse (item : string) =
    if item[item.Length - 1] = '-' then
      { Label = item.Substring (0, item.Length - 1); FocalLength = None }
    else
      let parts = item.Split "="
      { Label = parts[0]; FocalLength = Some (int parts[1]) }

type Boxes = ResizeArray<Lens> array

[<RequireQualifiedAccess>]
module Boxes =

  let empty () =
    Array.init 256 (fun _ -> ResizeArray<Lens> ())

  let power (boxes : Boxes) =
    (Seq.indexed boxes) |> Seq.sumBy (fun (ib, box) ->
      (Seq.indexed box) |> Seq.sumBy (fun (il, lens) ->
        (ib + 1) * (il + 1) * lens.FocalLength))
  
  let update (boxes : Boxes) (step : Step) =
    let box = boxes[Hash.calculate step.Label]
    let il = box |> Seq.tryFindIndex (fun l -> l.Label = step.Label)
    match step.FocalLength, il with
    | None, Some il -> box.RemoveAt il
    | Some len, Some il -> box[il] <- { Label = step.Label; FocalLength = len }
    | Some len, None -> box.Add { Label = step.Label; FocalLength = len }
    | _ -> ()
    boxes

      
module Puzzle2 =

  open System.IO

  let solve (input : string) =
    ((File.ReadAllLines input)[0]).Split ','
    |> Array.map Step.parse
    |> Array.fold Boxes.update (Boxes.empty ())
    |> Boxes.power