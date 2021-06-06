module Accumulate

let accumulate (func: 'a -> 'b) (input: 'a list): 'b list = 
    let rec accumulate' list acc =
        match list with
        | [] -> acc
        | head :: tail -> accumulate' tail (func(head) :: acc)
    accumulate' input [] |> List.rev

