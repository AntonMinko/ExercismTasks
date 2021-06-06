module Hamming

let distance (strand1: string) (strand2: string): int option =
    match strand1.Length = strand2.Length with
    | true -> List.map2 (fun s1 s2 -> if s1 <> s2 then 1 else 0) (Seq.toList strand1) (Seq.toList strand2)
                |> List.sum
                |> Some          
    | false -> None 