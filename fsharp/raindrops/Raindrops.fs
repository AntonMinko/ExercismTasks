module Raindrops

let soundsAsX (number: int) (str: string option) (x: int) (sound: string) =
    if (number % x = 0) then
        match str with
        | Some s -> Some(s + sound)
        | None -> Some(sound)
    else
        str

let soundsAs3 (number: int) (str: string option) =
    soundsAsX number str 3 "Pling"

let soundsAs5 (number: int) (str: string option) =
    soundsAsX number str 5 "Plang"

let soundsAs7 (number: int) (str: string option) =
    soundsAsX number str 7 "Plong"

let unknownSound (number: int) (str: string option) =
    match str with
    | Some s -> s
    | None -> number.ToString()

let convert (number: int): string = 
    None |> soundsAs3 number |> soundsAs5 number |> soundsAs7 number |> unknownSound number