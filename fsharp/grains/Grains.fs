module Grains

let square (n: int): Result<uint64,string> =
    if n < 1 || n > 64 then
        Error "square must be between 1 and 64"
    else
        1UL <<< (n-1)
        |> Ok

let total: Result<uint64,string> = 
    [1..64]
    |> List.map (fun i -> 1UL <<< (i-1))
    |> List.sum
    |> Ok
