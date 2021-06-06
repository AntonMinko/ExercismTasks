module Darts
open System

let score (x: double) (y: double): int =
    let radius = Math.Sqrt(x*x + y*y)
    match radius with
    | r when r <= 1.0 -> 10
    | r when r <= 5.0 -> 5
    | r when r <= 10.0 -> 1
    | _ -> 0