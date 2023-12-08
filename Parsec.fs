module AdventOfCode.Parsec

open System

type Parsec<'a> =
  string -> ('a * string) option

let getParsed (p : Parsec<'a>) (inp : string) =
  match p inp with
  | Some (a, _) -> a
  | None -> failwith "parse error"

let bind (p : Parsec<'a>) (f : 'a -> Parsec<'b>) : Parsec<'b> =
  fun input ->
    p input |> Option.bind (fun (a, inp) -> (f a) inp)

let retur (a : 'a) : Parsec<'a> =
  fun input -> Some (a, input)

let (>>=) = bind

let (>>) p1 p2 =
  p1 >>= fun a -> p2 >>= fun b -> retur (a, b)

let (>>.) p1 p2 =
  p1 >>= fun _ -> p2

let (.>>) p1 p2 =
  p1 >>= fun a -> p2 >>= fun _ -> retur a

let (<|>) (p1 : Parsec<'a>) (p2 : Parsec<'a>) : Parsec<'a> =
  fun input ->
    match p1 input with
    | Some _ as r -> r
    | None -> p2 input

let map (f : 'a -> 'b) (p : Parsec<'a>) : Parsec<'b> =
  fun input ->
    p input |> Option.map (fun (a, inp) -> (f a, inp))

let pWord (w : string) : Parsec<string> =
  fun input ->
    let input = input.TrimStart ()
    if input.StartsWith w then Some (w, input.Substring w.Length)
    else None

let pInt : Parsec<int> =
  fun input ->
    let input = input.TrimStart ()
    let mutable n = 0
    let mutable i = 0
    while i < input.Length && Char.IsDigit input[i] do
      n <- n * 10 + (int input[i] - int '0')
      i <- i + 1
    if i = 0 then None
    else Some (n, input.Substring i)

let pAtLeastOneSep sep p =
  let consWith pRem x = map (fun xs -> x :: xs) pRem <|> retur [ x ]
  let rec pTail input = ((pWord sep >>. p) >>= consWith pTail) input
  p >>= consWith pTail

let pAtLeastOne p =
  let consWith pRem x = map (fun xs -> x :: xs) pRem <|> retur [ x ]
  let rec pTail input = (p >>= consWith pTail) input
  p >>= consWith pTail