module AdventOfCode.Day24

open System.Numerics
open System

[<Struct>]
type V2 = { X : decimal; Y: decimal }

[<Struct>]
type V3 = { X : bigint; Y : bigint; Z : bigint }
with
  static member (+) (a : V3, b : V3) =
    { X = a.X + b.X; Y = a.Y + b.Y; Z = a.Z + b.Z }
  
  static member (-) (a : V3, b : V3) =
    { X = a.X - b.X; Y = a.Y - b.Y; Z = a.Z - b.Z }
  
  static member (*) (t : bigint, a : V3) =
    { X = t * a.X; Y = t * a.Y; Z = t * a.Z }

[<Struct>]
type M2 =
  { M11 : decimal; M12 : decimal
    M21 : decimal; M22 : decimal
  }
with
  static member (*) (a : M2, b : M2) =
    { M11 = a.M11 * b.M11 + a.M12 * b.M21
      M12 = a.M11 * b.M12 + a.M12 * b.M22;
      M21 = a.M21 * b.M11 + a.M22 * b.M21
      M22 = a.M21 * b.M12 + a.M22 * b.M22
    }
  
  static member (*) (a : M2, b : V2) =
    { X = a.M11 * b.X + a.M12 * b.Y
      Y = a.M21 * b.X + a.M22 * b.Y
    }
  
  static member det (a : M2) =
    a.M11 * a.M22 - a.M12 * a.M21
  
  static member inv (a : M2) =
    let d = M2.det a
    if d = 0m then None
    else
      Some <|
        { M11 = a.M22 / d; M12 = -a.M12 / d
          M21 = -a.M21 / d; M22 = a.M11 / d
        }

[<RequireQualifiedAccess>]
module BigInteger =

  let chinese (moduli : bigint seq) (numbers : bigint seq) =
    let modInv a m = BigInteger.ModPow(a, m - 2I, m)
    let prod = Seq.fold (*) 1I moduli
    let sum =
      (moduli, numbers)
      ||> Seq.map2 (fun m a -> let p = prod / m in a * (modInv p m) * p)
      |> Seq.sum
    sum % prod

  let isPrime (n : bigint) =
    if n < 2I then false
    elif n = 2I then true
    elif n % 2I = 0I then false
    else
      let mutable i = 3I
      let mutable answer = true
      while answer && i * i <= n do
        if n % i = 0I then answer <- false
        else i <- i + 2I
      answer


[<Struct>]
type Range = { Min : int; Max : int }

[<Struct>]
type Particle2 = { Pos : V2; Vel : V2 }

[<Struct>]
type Particle3 = { Pos : V3; Vel : V3 }

[<RequireQualifiedAccess>]
module Particle2 =

  let parse (line : string) : Particle2 =
    let parts = line.Split " @ "
    let ps = Array.map decimal (parts[0].Split ", ")
    let vs = Array.map decimal (parts[1].Split ", ")
    { Pos = { X = ps[0]; Y = ps[1] }; Vel = { X = vs[0]; Y = vs[1] } }

  let intersection (p1 : Particle2) (p2 : Particle2) =
    { M11 = p1.Vel.Y; M12 = -p1.Vel.X
      M21 = p2.Vel.Y; M22 = -p2.Vel.X
    }
    |> M2.inv
    |> Option.map (fun inv ->
      inv *
        { X = p1.Vel.Y * p1.Pos.X - p1.Vel.X * p1.Pos.Y
          Y = p2.Vel.Y * p2.Pos.X - p2.Vel.X * p2.Pos.Y
        })
  
  let inFuture (p : Particle2) (v : V2) =
    if p.Vel.X <> 0M then (v.X - p.Pos.X) / p.Vel.X > 0M
    else (v.Y - p.Pos.Y) / p.Vel.Y > 0M

[<RequireQualifiedAccess>]
module Particle3 =

  let parse (line : string) : Particle3 =
    let parts = line.Split " @ "
    let ps = Array.map BigInteger.Parse (parts[0].Split ", ")
    let vs = Array.map BigInteger.Parse (parts[1].Split ", ")
    { Pos = { X = ps[0]; Y = ps[1]; Z = ps[2] }
      Vel = { X = vs[0]; Y = vs[1]; Z = vs[2]}
    }

module Puzzle1 =

  open System.IO

  let private areaMin = 200000000000000M
  let private areaMax = 400000000000000M

  let private inRange (v : V2) =
    areaMin <= v.X && v.X <= areaMax && areaMin <= v.Y && v.Y <= areaMax

  let solve (input : string) =
    let paricles = Array.map Particle2.parse (File.ReadAllLines input)
    let mutable result = 0
    for i = 0 to paricles.Length - 1 do
      for j = i + 1 to paricles.Length - 1 do
        let pi = paricles[i]
        let pj = paricles[j]
        match Particle2.intersection pi pj with
        | Some v ->
          if inRange v && Particle2.inFuture pi v && Particle2.inFuture pj v then
            result <- result + 1
        | None -> ()
    result
    
module Puzzle2 =

  open System.IO

  let sol dim particles =
    let moduli = ResizeArray<bigint> ()
    let numbers = ResizeArray<bigint> ()
    let getChinese v0 (dim : V3 -> bigint) =
      for p in particles do
        let m = v0 - dim p.Vel
        if BigInteger.isPrime m && Seq.forall ((<>) m) moduli then
          moduli.Add m
          numbers.Add (dim p.Pos)
      let ch = BigInteger.chinese moduli numbers
      moduli.Clear (); numbers.Clear ()
      ch
    let verify v0 (dim : V3 -> bigint) ch =
      not (Seq.exists
        (fun p ->
          let dv = v0 - dim p.Vel
          let dx = abs (dim p.Pos - ch)
          if dv = 0I then dx <> 0I else dx % dv <> 0I)
        particles)
    let rec loop v0 =
      if v0 > 10000I then
        failwith "no solution found"
      else
        let ans = getChinese v0 dim
        if verify v0 dim ans then ans
        else loop (v0 + 1I)
    loop -10000I

  let solve (input : string) =
    let particles = Array.map Particle3.parse (File.ReadAllLines input)
    let x = sol (_.X) particles
    let y = sol (_.Y) particles
    let z = sol (_.Z) particles
    x + y + z