module IsbnVerifier
open System

let isValid isbn =
    let validateLength cleanIsbn = 
        match cleanIsbn |> Seq.length with
        | 10 -> cleanIsbn |> Ok
        | _ -> Error false

    let convertNumbers cleanIsbn =
        cleanIsbn
        |> Seq.map (fun ch -> if ch = 'X' then 10 else int ch - int '0')
        |> Ok

    let validateNumbers numbers =
        let isValid = 
            numbers
            |> Seq.mapi (fun i x -> match x with 
                                    | _ when i < 9 && 0 <= x && x <= 9 -> x
                                    | _ when i = 9 && 0 <= x && x <= 10 -> x
                                    | _ -> -1)
            |> Seq.exists (fun x -> x = -1)
            |> not
        if isValid then numbers |> Ok else Error false

    let validateIsbn numbers =
        let checksum = 
            numbers
            |> Seq.mapi (fun i x -> x * (10 - i))
            |> Seq.sum
        if checksum % 11 = 0 then true else false
        |> Ok

    let result = 
        isbn
        |> Seq.filter (fun ch -> ch <> '-')
        |> validateLength
        |> Result.bind convertNumbers
        |> Result.bind validateNumbers
        |> Result.bind validateIsbn

    match result with
    | Ok isValid -> isValid
    | _ -> false
