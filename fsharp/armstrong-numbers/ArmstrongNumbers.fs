module ArmstrongNumbers

let isArmstrongNumber number =
    let rec toDigits num =
        match num with
        | num when num < 10 -> [num]
        | num -> (num % 10) :: toDigits (num / 10)

    let digits = number |> toDigits
    let power = digits |> List.length
    
    digits 
    |> List.sumBy (fun d -> pown d power)
    |> (=) number
