module Change
open System.Collections.Generic

let findFewestCoins (coins: int list) (target: int) =
    let coinsOrdered = coins |> List.sortDescending
    let memo = Dictionary<int, int list>()
    let rec findCoins' remaining =
        match remaining with
        | _ when remaining < 0 -> None
        | _ when remaining = 0 -> [] |> Some
        | _ when memo.ContainsKey(remaining) -> memo.[remaining] |> Some
        | _ ->
            coinsOrdered
            |> List.map (fun coin -> (coin, findCoins' (remaining - coin)))
            |> List.iter (fun (coin, change) ->
                            match change with
                            | Some l when (memo.ContainsKey(remaining) |> not) || memo.[remaining].Length > l.Length + 1 ->
                                memo.[remaining] <- coin::l
                            | _ -> ())
            match memo.ContainsKey remaining with
            | true -> memo.[remaining] |> List.sort |> Some
            | _ -> None
    findCoins' target
        
        