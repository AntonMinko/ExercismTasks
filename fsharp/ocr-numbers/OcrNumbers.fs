module OcrNumbers
open System

let private zero = 
    [ " _ ";
      "| |";
      "|_|";
      "   " ]
let private one = 
    [ "   ";
      "  |";
      "  |";
      "   " ]
let two = 
    [ " _ ";
      " _|";
      "|_ ";
      "   " ]
let three = 
    [ " _ ";
      " _|";
      " _|";
      "   " ]
let four = 
    [ "   ";
      "|_|";
      "  |";
      "   " ]
let five = 
    [ " _ ";
      "|_ ";
      " _|";
      "   " ]
let six = 
    [ " _ ";
      "|_ ";
      "|_|";
      "   " ]
let seven = 
    [ " _ ";
      "  |";
      "  |";
      "   " ]
let eight = 
    [ " _ ";
      "|_|";
      "|_|";
      "   " ]
let nine = 
    [ " _ ";
      "|_|";
      " _|";
      "   " ]

let private numbers = [
    (0, zero);
    (1, one);
    (2, two);
    (3, three);
    (4, four);
    (5, five);
    (6, six);
    (7, seven);
    (8, eight);
    (9, nine)]

let private isEqualNumber (num1: string list) (num2: string list) =
    List.map2 (fun s1 s2 -> s1 = s2) num1 num2
    |> List.exists (fun b -> not b)
    |> not

let private ocrNumber (num: string list) =
    let num =
        numbers
        |> List.map (fun (n, nStr) -> if isEqualNumber num nStr then Some(n) else None)
        |> List.tryFind (fun optN -> optN.IsSome)
    match num with
    | Some n -> n.Value.ToString()
    | None -> "?"

let rec splitString(num: string list) =
    if (num |> List.exists (fun str -> str.Length = 0)) then []
    else
        let n = 
            num
            |> List.map (fun str -> str.Substring(0, 3))
        let rest =
            num
            |> List.map (fun str -> str.Substring(3))
        n::splitString(rest)

let parseLine (line: string list) =
    splitString line 
        |> List.map ocrNumber
        |> String.concat ""

let splitLines (input: string list) =
    input
    |> List.chunkBySize 4

let convert (input: string list) =
    if (input.Length % 4 <> 0) then None
    else if (input |> List.exists (fun str -> str.Length % 3 <> 0)) then None
    else
        splitLines input
        |> List.map parseLine
        |> String.concat ","
        |> Some