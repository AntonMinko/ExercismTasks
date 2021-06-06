module NucleotideCount
open System.Collections.Generic

let nucleotideCounts (strand: string): Option<Map<char, int>> = 
    let getValue key (dictionary: IDictionary<char, int>) = 
        match dictionary.TryGetValue key with
        | true, v -> v
        | _ -> 0
    let items = Seq.toList strand |> List.countBy id |> dict
    let a = items |> getValue 'A'
    let c = items |> getValue 'C'
    let g = items |> getValue 'G'
    let t = items |> getValue 'T'
    if a + c + g + t = strand.Length then
        Map.empty
            .Add('A', a)
            .Add('C', c)
            .Add('G', g)
            .Add('T', t)
        |> Some
    else
        None
