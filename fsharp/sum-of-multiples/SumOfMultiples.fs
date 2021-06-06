module SumOfMultiples

let sum (numbers: int list) (upperBound: int): int =
    numbers
    |> List.filter (fun m -> m <> 0)
    |> List.map (fun n -> [n..n..(upperBound-1)])
    |> List.concat
    |> List.distinct
    |> List.sum