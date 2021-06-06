module RotationalCipher
open System

let rotate shiftKey text =
    let alphabet = "abcdefghijklmnopqrstuvwxyz"
    let getKey (ch: char) = (int ch - int 'a' + shiftKey) % 26
    let encode (ch: char) =
        match ch with
        | _ when Char.IsLetter ch && Char.IsLower ch -> alphabet.[getKey ch]
        | _ when Char.IsLetter ch -> alphabet.[getKey (Char.ToLower ch)] |> Char.ToUpper
        | _ -> ch
    
    text
    |> Seq.toArray
    |> Array.map encode
    |> String