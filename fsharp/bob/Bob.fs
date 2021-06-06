module Bob
open System.Linq
open System

type private SentenceType =
    | Empty
    | Shouting
    | Question
    | ShoutingQuestion
    | Other
    
let response (input: string): string =
    let getSentenceType (input: string) =
        let isAllCapitals = input.Any(fun ch -> Char.IsLetter(ch)) && input = input.ToUpper()
        if String.IsNullOrEmpty input then 
            Empty
        else if input.EndsWith('?') then
            if isAllCapitals then
                ShoutingQuestion
            else
                Question
        else if isAllCapitals then
            Shouting
        else
            Other

    match getSentenceType (input.Trim()) with
    | Empty -> "Fine. Be that way!" 
    | Shouting -> "Whoa, chill out!"
    | Question -> "Sure."
    | ShoutingQuestion -> "Calm down, I know what I'm doing!"
    | Other -> "Whatever."
