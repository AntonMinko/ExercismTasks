module RnaTranscription
open System

let toRna (dna: string): string =
    let transcribe (dnaItem: char) =
        match dnaItem with
            | 'G' -> 'C'
            | 'C' -> 'G'
            | 'T' -> 'A'
            | 'A' -> 'U'
            | _ -> failwith "Unexpected element"
    
    dna
    |> Seq.map transcribe
    |> String.Concat