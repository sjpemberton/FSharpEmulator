module Utils

let toBools (ints:int array) = 
    let intToBool = function
        | 1 -> true
        | _ -> false
    ints
    |> Array.map intToBool

