module Series
open System

let slices (str: string) length =
    if (str.Length < length || length <= 0) then None
    else
        str 
        |> Seq.windowed length 
        |> Seq.map String
        |> Seq.toList
        |> Some