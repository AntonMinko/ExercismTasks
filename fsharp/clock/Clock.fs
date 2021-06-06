module Clock

type Clock =   
    {
        Hours : int;
        Minutes : int
    }

let normalizeAndCreate hours minutes = 
    let hours = hours + (minutes / 60)
    let hours = if minutes < 0 && minutes % 60 <> 0 then hours - 1 else hours
    let minutes = minutes % 60
    let minutes = if minutes < 0 then minutes + 60 else minutes
    let hours = hours % 24
    let hours = if hours < 0 then hours + 24 else hours
    { Hours = hours; Minutes = minutes }

let create hours minutes = 
    normalizeAndCreate hours minutes

let add minutes clock =
    normalizeAndCreate clock.Hours (clock.Minutes + minutes)

let subtract minutes clock =
    normalizeAndCreate clock.Hours (clock.Minutes - minutes)

let display clock = sprintf "%02i:%02i" clock.Hours clock.Minutes

