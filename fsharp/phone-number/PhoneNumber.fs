module PhoneNumber
open System

let clean (input: string) =
    let removeAllowedPunctuation chars =
        chars 
        |> Seq.filter (fun (ch: char) -> not (" ()-+.".Contains(ch)))
        |> Ok
    
    let checkAllDigits chars =
        match chars |> Seq.forall Char.IsDigit with
        | true -> chars |> Ok
        | false -> if chars |> Seq.exists Char.IsLetter then
                       Error "letters not permitted"
                   else
                       Error "punctuations not permitted"
    
    let checkLength chars =
        match chars |> Seq.length with
        | 10 | 11 -> chars |> Ok
        | l when l > 11 -> Error "more than 11 digits"
        | _ -> Error "incorrect number of digits"
    
    let checkCountryCode chars =
        match chars |> Seq.length with
        | 11 -> if chars |> Seq.head |> (=) '1' then
                    chars |> Seq.skip 1 |> Ok
                else
                    Error "11 digits must start with 1"
        | _ -> chars |> Ok
    
    let checkAreaCode chars =
            match chars |> Seq.head |> Char.ToString |> Int32.Parse with
            | d when 2 <= d && d <= 9 -> chars |> Ok
            | d when d = 0 -> Error "area code cannot start with zero"
            | _ -> Error "area code cannot start with one"

    let checkExchangeCode chars =
        match chars |> Seq.skip 3 |> Seq.head |> Char.ToString |> Int32.Parse with
        | d when 2 <= d && d <= 9 -> chars |> Ok
        | d when d = 0 -> Error "exchange code cannot start with zero"
        | _ -> Error "exchange code cannot start with one"
    
    let toLong chars =
        chars 
        |> Seq.toArray
        |> String
        |> UInt64.Parse
        |> Ok
    
    input
    |> removeAllowedPunctuation
    |> Result.bind checkAllDigits
    |> Result.bind checkLength
    |> Result.bind checkCountryCode
    |> Result.bind checkAreaCode
    |> Result.bind checkExchangeCode
    |> Result.bind toLong