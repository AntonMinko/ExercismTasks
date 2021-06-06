module Pangram
open System

let isPangram (input: string): bool =
    input.ToLowerInvariant()
    |> Seq.filter Char.IsLetter
    |> Seq.distinct
    |> Seq.length = 26
