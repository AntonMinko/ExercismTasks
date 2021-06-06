module BeerSong
open System

let recite startBottles takeDown =
    let getFirstLine bottles =
        match bottles with
        | 1 -> ("1 bottle", "1 bottle")
        | 0 -> ("No more bottles", "no more bottles")
        | _ -> ($"{bottles} bottles", $"{bottles} bottles")
        |> fun (p1, p2) -> String.Format("{0} of beer on the wall, {1} of beer.", p1, p2)

    let getSecondLine bottles =
        match bottles with
        | 2 -> $"Take one down and pass it around, 1 bottle"
        | 1 -> $"Take it down and pass it around, no more bottles"
        | 0 -> $"Go to the store and buy some more, 99 bottles"
        | _ -> $"Take one down and pass it around, {bottles-1} bottles"
        |> fun str -> str + " of beer on the wall."

    let getVerse bottles (isLastVerse: bool) = 
        let verse = [ getFirstLine bottles; getSecondLine bottles ]
        if isLastVerse then verse else verse @ [""]

    [for i in 0..(takeDown-1) -> startBottles-i]
    |> List.map (fun bottles -> getVerse bottles (bottles = (startBottles - takeDown + 1)))
    |> List.concat