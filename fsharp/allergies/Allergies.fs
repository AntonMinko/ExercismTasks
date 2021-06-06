﻿module Allergies

open System

[<Flags>]
type Allergen =
    | Eggs         = 0b00000001
    | Peanuts      = 0b00000010
    | Shellfish    = 0b00000100
    | Strawberries = 0b00001000
    | Tomatoes     = 0b00010000
    | Chocolate    = 0b00100000
    | Pollen       = 0b01000000
    | Cats         = 0b10000000

let allergicTo (codedAllergies: int) (allergen: Allergen) =
    codedAllergies &&& (int) allergen <> 0

let list codedAllergies =
    Enum.GetValues(typeof<Allergen>) :?> (Allergen [])
    |> Array.toList
    |> List.filter (fun n -> (int) n &&& codedAllergies <> 0)