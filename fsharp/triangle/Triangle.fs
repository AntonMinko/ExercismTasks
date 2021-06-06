module Triangle

let private isTriangle (triangle: float list) =
    if (triangle |> List.exists (fun side -> side <= 0.0)) then false
    else
        let sorted = triangle |> List.sort
        sorted.[0] + sorted.[1] > sorted.[2]

let private uniqueSides (triangle: float list) =
    (triangle |> List.distinct).Length

let equilateral triangle =
    if isTriangle triangle
    then uniqueSides triangle = 1
    else false

let isosceles triangle =
    if isTriangle triangle
    then uniqueSides triangle <= 2
    else false

let scalene (triangle: float list) =
    if isTriangle triangle
    then uniqueSides triangle = 3
    else false