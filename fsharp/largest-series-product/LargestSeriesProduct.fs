module LargestSeriesProduct
open System

let largestProduct (input: string) seriesLength : int option =
    match seriesLength with
    | _ when seriesLength < 0 -> None
    | _ when seriesLength = 0 -> Some 1
    | _ when seriesLength > input.Length -> None
    | _ when input |> Seq.forall Char.IsDigit |> not -> None
    | _ -> input
           |> Seq.map (fun ch -> Char.GetNumericValue(ch) |> int)
           |> Seq.windowed seriesLength
           |> Seq.map (fun arr -> arr |> Array.fold (fun acc n -> acc * n) 1)
           |> Seq.max
           |> Some