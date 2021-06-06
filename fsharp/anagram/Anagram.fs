module Anagram
open System

let private sortLetters (word: string) =
    word.ToLower()
    |> Seq.toArray
    |> Array.sort

let findAnagrams (sources: string list) (target: string) =
    let targetLowered = target.ToLower()
    let targetOrdered = target |> sortLetters

    let isAnagram (word: string) =
        word.ToLower() <> targetLowered && (word |> sortLetters) = targetOrdered
        
    sources |> List.filter isAnagram