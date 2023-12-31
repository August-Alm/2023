module AdventOfCode.Parsec

open System

type Parsec<'a> =
  string -> ('a * string) option

let getParsed (p : Parsec<'a>) (inp : string) =
  match p inp with
  | Some (a, _) -> a
  | None -> failwith "parse error"

let bind (p : Parsec<'a>) (f : 'a -> Parsec<'b>) : Parsec<'b> =
  Option.bind (fun (a, inp) -> (f a) inp) << p

let retur (a : 'a) : Parsec<'a> =
  fun input -> Some (a, input)

let (>>=) = bind

let (.>>.) p1 p2 =
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
  Option.map (fun (a, inp) -> (f a, inp)) << p

// Skips leading whitespace!
let pWord (w : string) : Parsec<string> =
  fun input ->
    let input = input.TrimStart ()
    if input.StartsWith w then Some (w, input.Substring w.Length)
    else None

// Skips leading whitespace!
let pAnyChar : Parsec<char> =
  fun input ->
    let input = input.TrimStart ()
    match input.Length with
    | 0 -> None
    | 1 -> Some (input[0], "")
    | _ -> Some (input[0], input.Substring 1)

// Skips leading whitespace! Should really be named `pLettersOrDigits`.
let pLetters : Parsec<string> =
  fun input ->
    let input = input.TrimStart ()
    let mutable i = 0
    while i < input.Length && Char.IsLetterOrDigit input[i] do
      i <- i + 1
    if i = 0 then None
    else Some (input.Substring (0, i), input.Substring i)

// Skips leading whitespace!
let pChar (c : char) : Parsec<char> =
  fun input ->
    match pAnyChar input with
    | Some (c', input') when c = c' -> Some (c, input')
    | c' -> printfn $"{c'} <> {c}"; None

// Skips leading whitespace!
let pLong : Parsec<int64> =
  fun input ->
    let input = input.TrimStart ()
    let mutable n = 0L
    let mutable i = 0
    let mutable sign = 0
    if input.Length > 0 then
      if input[0] = '-' then
        sign <- -1
        i <- 1
      if input[0] = '+' then
        sign <- 1
        i <- 1
    while i < input.Length && Char.IsDigit input[i] do
      n <- n * 10L + (int64 input[i] - int64 '0')
      i <- i + 1
    match sign, i with
    | 0, 0 | 1, 1 | -1, 1 -> None
    | -1, _ -> Some (-n, input.Substring i)
    | _, _ -> Some (n, input.Substring i)

// Skips leading whitespace!
let pInt = map int pLong

let pAtLeastOneSep sep p =
  let consWith pRem x = map (fun xs -> x :: xs) pRem <|> retur [ x ]
  let rec pTail input = ((pWord sep >>. p) >>= consWith pTail) input
  p >>= consWith pTail

let pAtLeastOne p =
  let consWith pRem x = map (fun xs -> x :: xs) pRem <|> retur [ x ]
  let rec pTail input = (p >>= consWith pTail) input
  p >>= consWith pTail