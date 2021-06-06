module WordCount
open System

type CharType =
| Letter
| Digit
| Apostrophe
| Delimiter

let getCharType (ch: char) =
    match ch with
    | c when Char.IsLetter(c) -> Letter
    | c when Char.IsDigit(c) -> Digit
    | c when c = ''' -> Apostrophe
    | _ -> Delimiter

let split (phrase: string) =
    let delimiters = phrase |> Seq.filter (fun ch -> getCharType ch = Delimiter) |> Seq.toArray
    
    phrase.Split(delimiters)
    |> Array.toList
    |> List.map (fun word -> word.Trim('''))
    |> List.filter (fun word -> String.IsNullOrEmpty(word) |> not)

let countWords (phrase: string) =
    split(phrase.ToLowerInvariant())
    |> List.countBy id
    |> Map.ofList