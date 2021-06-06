module Acronym
open System

let abbreviate (phrase: string) =
    let delimiters = 
        phrase
        |> Seq.filter (fun ch -> (Char.IsLetter(ch) || ch = ''') |> not)
        |> Seq.distinct
        |> Seq.toArray
    
    phrase.Split(delimiters)
    |> Array.filter(fun w -> w.Length > 0)
    |> Array.map (fun w -> Char.ToUpper w.[0])
    |> String