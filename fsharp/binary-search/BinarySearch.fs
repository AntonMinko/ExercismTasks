module BinarySearch

let find (input: int array) value =
    let rec find' l r =
        if (l > r) then None
        else
            let mid = (l + r) / 2
            match input.[mid] with
            | m when m = value -> Some mid
            | m when m < value -> find' (mid+1) r
            | m when m > value -> find' l (mid-1)
            | _ -> failwith "Unreachable code. Makig compiler happy."
    
    find' 0 (input.Length - 1)