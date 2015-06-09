module Utils

open ArithmeticLogicUnit

let toBools (ints:int array) = 
    let intToBool = function
        | 1 -> true
        | _ -> false
    ints
    |> Array.map intToBool

let toInts (bools:bool array) = 
    let boolToInt = function
        | true -> 1
        | _ -> 0
    bools
    |> Array.map boolToInt

let toBinary i =
    let rec convert i acc =
        match i with
        | _ when i > 0 -> (i % 2) :: (convert (i/2) acc)
        | _ -> acc
    convert i []
    |> List.rev

let toDecimal b =
    let rec convert b i acc =
        match b with
        | h::t -> ((float h)*2.0**i) + (convert t (i+1.0) acc)
        |[] -> acc
    convert (b |> List.rev) 0.0 0.0
    |> int

let convertToTwosCompliment b =
    let rec convert b acc =
        match b with
        | h::t ->
            match h with
            | 1 -> 0 :: convert t acc
            | _ -> 1 :: convert t acc
        |[] -> acc
    convert b []

let padBits length (bits:int list) =
    let pad = length - bits.Length
    [for i in 1 .. pad -> 0] @ bits

//Doesn't handle overflow
let toTwosCompliment i b =
    let convertToBinary = function
        | _ when i < 0 -> 
            abs i |> toBinary
            |> padBits b
            |> convertToTwosCompliment
            |> List.toArray
            |> toBools
            |> Incrementer
            |> toInts
            |> Array.toList
        | _ -> 
            i |> toBinary
            |> padBits b
    convertToBinary i
    
let toDecimalFromTC (b:int list) =
    let msb = b.[0]
    let r = b |> toDecimal
    (if msb = 0 then r else -r)