module AtbashCipher
open System

let private alphabet = "abcdefghijklmnopqrstuvwxyz"
let private cipher = "zyxwvutsrqponmlkjihgfedcba"

let encode (str: string) =
    let encodeChar (ch: char) = 
        match Char.ToLower ch with
        | c when Char.IsLetter c -> cipher.[int c - int 'a']
        | _ -> ch
    str
    |> Seq.filter Char.IsLetterOrDigit
    |> Seq.map encodeChar
    |> Seq.chunkBySize 5
    |> Seq.map String
    |> String.concat " "

let decode (str: string) =
    let decodeChar (ch: char) =
        match ch with
        | _ when Char.IsLetter ch -> alphabet.[25 - (int ch - int 'a')]
        | _ -> ch
    str
    |> Seq.filter Char.IsLetterOrDigit
    |> Seq.map decodeChar
    |> Seq.toArray
    |> String