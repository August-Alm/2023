module AdventOfCode.Parser

open System

type Parser<'a> =
  string -> ('a * string) option

let bind (p : Parser<'a>) (f : 'a -> Parser<'b>) : Parser<'b> =
  fun input ->
    p input |> Option.bind (fun (a, inp) -> (f a) inp)

let retur (a : 'a) : Parser<'a> =
  fun input -> Some (a, input)

let (>>=) = bind

let (>>) p1 p2 =
  p1 >>= fun a -> p2 >>= fun b -> retur (a, b)

let (>>.) p1 p2 =
  p1 >>= fun _ -> p2

let (.>>) p1 p2 =
  p1 >>= fun a -> p2 >>= fun _ -> retur a

let (<|>) (p1 : Parser<'a>) (p2 : Parser<'a>) : Parser<'a> =
  fun input ->
    match p1 input with
    | Some _ as r -> r
    | None -> p2 input

let map (f : 'a -> 'b) (p : Parser<'a>) : Parser<'b> =
  fun input ->
    p input |> Option.map (fun (a, inp) -> (f a, inp))

let pWord (w : string) : Parser<string> =
  fun input ->
    let input = input.TrimStart ()
    if input.StartsWith w then Some (w, input.Substring w.Length)
    else None

let pInt : Parser<int> =
  fun input ->
    let input = input.TrimStart ()
    let mutable n = 0
    let mutable i = 0
    while i < input.Length && Char.IsDigit input[i] do
      n <- n * 10 + (int input[i] - int '0')
      i <- i + 1
    if i = 0 then None
    else Some (n, input.Substring i)

let pAtLeastOne sep p =
  let consWith pRem x = map (fun xs -> x :: xs) pRem <|> retur [ x ]
  let rec pTail input = ((pWord sep >>. p) >>= consWith pTail) input
  p >>= consWith pTail
