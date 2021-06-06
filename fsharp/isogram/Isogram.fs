module Isogram
open System

let isIsogram (str: string) =
    str.ToLower()
    |> Seq.filter Char.IsLetter
    |> Seq.countBy id
    |> Seq.forall (fun (_, count) -> count = 1)