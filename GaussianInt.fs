module AdventOfCode.GaussianInt

[<Struct>]
type GaussianInt = { X : int; Y : int }
with
  static member Zero = { X = 0; Y = 0 }
  static member One = { X = 1; Y = 0 }
  static member ImaginaryOne = { X = 0; Y = 1 }

  static member (+) (a, b) = { X = a.X + b.X; Y = a.Y + b.Y }
  
  static member (-) (a, b) = { X = a.X - b.X; Y = a.Y - b.Y }
  
  static member (~-) a = { X = -a.X; Y = -a.Y }
  
  static member (*) (a, b) =
    { X = a.X * b.X - a.Y * b.Y; Y = a.X * b.Y + a.Y * b.X }
  
  static member (/) (a, b) =
    failwith "gaussian integers only form a ring"
  
  static member L1Norm a = abs a.X + abs a.Y

  static member L2Norm a =
    System.Math.Sqrt ((double a.X) * (double a.X) + (double a.Y) * (double a.Y))

  static member ofInt n = { X = n; Y = 0 }
