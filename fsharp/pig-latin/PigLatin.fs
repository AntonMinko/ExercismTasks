module PigLatin

let translate (input: string) =
    let isVowel (ch: char) =
        match ch with
        | 'a' -> true
        | 'e' -> true
        | 'o' -> true
        | 'i' -> true
        | 'u' -> true
        | _ -> false

    let firstVowelIndex (word: string) =
        let vowelInd =
            word |> Seq.tryFindIndex isVowel
        let yInd = word.IndexOf('y')
        if vowelInd.IsNone || (yInd > 0 && yInd < vowelInd.Value) then yInd else vowelInd.Value

    let translateWord (word: string) =
        let shift = 
            if word.StartsWith("xr") || word.StartsWith("yt") then 0
            else if word.Length = 2 && word.[1] = 'y' then 1
            else
                let vowelInd = firstVowelIndex word
                if vowelInd > 0 && word.IndexOf("qu") = vowelInd - 1 then vowelInd + 1 
                else vowelInd
        word.Substring(shift) + word.Substring(0, shift) + "ay"

    input.Split(" ")
    |> Seq.map translateWord
    |> String.concat " "
