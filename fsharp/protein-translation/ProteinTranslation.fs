module ProteinTranslation
open System

type Proteins =
        Methionine
        | Phenylalanine
        | Leucine
        | Serine
        | Tyrosine
        | Cysteine
        | Tryptophan
        | STOP

let codonsToProteins str =
    match str with
    | "AUG" -> Methionine
    | "UUU" | "UUC" -> Phenylalanine
    | "UUA" | "UUG" -> Leucine
    | "UCU" | "UCC" | "UCA" | "UCG" -> Serine
    | "UAU" | "UAC" -> Tyrosine
    | "UGU" | "UGC" -> Cysteine
    | "UGG" -> Tryptophan
    | "UAA" | "UAG" | "UGA" -> STOP
    | _ -> failwith "Unknown sequence"

let proteins rna =
    rna
    |> Seq.chunkBySize 3
    |> Seq.map String
    |> Seq.map codonsToProteins
    |> Seq.takeWhile (fun protein -> protein <> STOP)
    |> Seq.map (fun protein -> protein.ToString())
    |> Seq.toList