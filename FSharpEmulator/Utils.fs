module Utils

open ArithmeticLogicUnit

let stringToInt = function | "1" ->  1 | _ -> 0
let intToBool = function | 1 -> true | _ -> false
let boolToInt = function | true -> 1  | _ -> 0
let intToString = function | 1 -> "1" | _ -> "0"

//let stringToInts = Array.map stringToInt
//let intsToBools ints = ints |> Array.map intToBool
//let boolsToInts bools=  bools |> Array.map boolToInt

let intsToString = Array.map intToString >> String.concat ""
let stringToBool = stringToInt >> intToBool
let boolToString = boolToInt >> intToString

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
            |> Array.map intToBool
            |> Incrementer
            |>  Array.map boolToInt
            |> Array.toList
        | _ -> 
            i |> toBinary
            |> padBits b
    convertToBinary i
    
let toDecimalFromTC (b:int list) =
    let msb = b.[0]
    let r = b |> toDecimal
    (if msb = 0 then r else -r)

open System

let andComparison = "|   a   |   b   |  out  |
|   0   |   0   |   0   |
|   0   |   1   |   0   |
|   1   |   0   |   0   |
|   1   |   1   |   1   |"

let parseFile (s:string)  = 
    s.Split([|'\n'|])
    |> Seq.map (fun s -> s.Split([|"|"|],StringSplitOptions.RemoveEmptyEntries)
                            |> Array.map(fun s -> s.Trim())
                            |> Array.toList)

let rec createDict (matrix: string list list) (cols:string list) =
    match matrix with
    | (row::rest) -> 
        let argDict = row 
                      |> List.mapi (fun i x -> (cols.[i], x)) 
                      |> Map.ofSeq
        [argDict] @ createDict rest cols
    | _ -> [] 

let createTestData matrix =
    createDict (List.tail matrix) (List.head matrix)
   
let executeTests func data =
    let testData = createTestData data
    let rec execute func (testData: Map<string,string> list) = 
        match testData with
        | case::rest ->
            sprintf "result = %b - expecting %b \n" (func case) (stringToBool case.["out"]) + execute func rest
        |[] -> "Done"
    execute func testData

let andTest (case: Map<string,string>) =
    And (stringToBool case.["a"]) (stringToBool case.["b"])