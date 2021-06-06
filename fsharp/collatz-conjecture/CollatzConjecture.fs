module CollatzConjecture

let steps (number: int): int option = 
    let rec makeStep (num: int) =
        if num = 1 then
            0
        else
            let stepsAhead = 
                if num % 2 = 0 then
                    makeStep (num / 2)
                else
                    makeStep (3 * num + 1)
            stepsAhead + 1
    
    if number <= 0 then
        None
    else
        Some(makeStep number)
