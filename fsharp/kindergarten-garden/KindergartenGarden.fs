module KindergartenGarden
open System

type Plant =
| Grass
| Clover
| Radishes
| Violets

let plants (diagram: string) (student: string) =
    let charToPlant ch =
        match ch with
        | 'g' -> Grass
        | 'c' -> Clover
        | 'r' -> Radishes
        | 'v' -> Violets
        | _ -> failwith "Unexpected plant"

    let getPlants skipCount line =
        line
        |> Seq.skip skipCount
        |> Seq.take 2
        |> Seq.map Char.ToLower
        |> Seq.map charToPlant

    let firstChar =
        student 
        |> Seq.head
        |> Char.ToLower
    let skipCount = ((int) firstChar - (int) 'a') * 2

    diagram.Split('\n')
    |> Seq.map (getPlants skipCount) 
    |> Seq.concat
    |> Seq.toList