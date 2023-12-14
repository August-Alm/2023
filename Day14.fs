module AdventOfCode.Day14

[<Struct>]
type GaussianInt = { X : int; Y : int }
with
  static member One = { X = 1; Y = 0 }
  static member ImaginaryOne = { X = 0; Y = 1 }

  static member (+) (a, b) =
    { X = a.X + b.X; Y = a.Y + b.Y }
  
  static member (-) (a, b) =
    { X = a.X - b.X; Y = a.Y - b.Y }
  
  static member (*) (a, b) =
    { X = a.X * b.X - a.Y * b.Y; Y = a.X * b.Y + a.Y * b.X }
  
  static member (/) (a, b) =
    failwith "gaussian integers only form a ring"


[<RequireQualifiedAccess>]
module Direction =
  let East = GaussianInt.One
  let North = GaussianInt.ImaginaryOne
  
