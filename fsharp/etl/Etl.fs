module Etl
open System

let transform (scoresWithLetters: Map<int, char list>): Map<char, int> =
    scoresWithLetters
    |> Map.toSeq
    |> Seq.map (fun (score, chars) -> 
        chars 
        |> Seq.map (fun ch -> Char.ToLower ch, score))
    |> Seq.concat
    |> Map.ofSeq
