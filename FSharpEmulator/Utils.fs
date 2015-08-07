module Utils

open ArithmeticLogicUnit
open System.IO
open System

let stringToInt = function 
    | "1" -> 1s
    | _ -> 0s

let charToInt = function 
    | '1' -> 1s
    | _ -> 0s

let intToBool = function 
    | 1s -> true
    | _ -> false

let boolToInt = function 
    | true -> 1s
    | _ -> 0s

let intToString = function 
    | 1s -> "1"
    | _ -> "0"

//let stringToInts = Array.map stringToInt
//let intsToBools ints = ints |> Array.map intToBool
//let boolsToInts bools=  bools |> Array.map boolToInt
let intsToString = Array.map intToString >> String.concat ""
let boolsToString = Array.map boolToInt >> intsToString
let stringToBool = stringToInt >> intToBool
let boolToString = boolToInt >> intToString

let toBinary i = 
    let rec convert i acc = 
        match i with
        | _ when i > 0s -> (i % 2s) :: convert (i / 2s) acc
        | _ -> acc
    convert i [] |> List.rev |> List.toArray

let flipBits b = //Can just use ~~~ if using ints!
    let rec convert b acc = 
        match b with
        | h :: t -> 
            match h with
            | 1s -> 0s :: convert t acc
            | _ -> 1s :: convert t acc
        | [] -> acc
    convert (b |> List.ofSeq) []
    |> List.toArray

let padBits length (bits : int16 array) =
    let padding = [| for i in 1..(length - bits.Length) -> 0s |]
    Array.concat [|padding; bits|]

//Doesn't handle overflow
let toTwosCompliment i b = 
    match i with
    | _ when i < 0s -> 
        abs i |> int16
        |> toBinary
        |> padBits b
        |> flipBits 
        |> Increment
    | _ -> 
        i
        |> toBinary
        |> padBits b

let toBase10 b = 
    let rec convert b i acc = 
        match b with
        | h :: t -> float h * 2.0 ** i + convert t (i + 1.0) acc
        | [] -> acc
    convert (b |> Array.rev |> Array.toList) 0.0 0.0 |> int16

let toDecimal b (binary : int16 array) =
    match binary.[0] with
    | 0s -> binary |> toBase10
    | _ -> 
        -(binary
        |> padBits b
        |> flipBits 
        |> Increment
        |> toBase10)
        

let parseFile path = 
    File.ReadAllLines path
    |> Seq.map (fun s -> 
           s.Split([| "|" |], StringSplitOptions.RemoveEmptyEntries)
           |> Array.map (fun s -> s.Trim())
           |> Array.toList)
    |> Seq.toList

let rec createMap matrix (cols : string list) = 
    match matrix with
    | row :: rest -> 
        [ row
          |> List.mapi (fun i x -> (cols.[i], x))
          |> Map.ofSeq ]
        @ createMap rest cols
    | _ -> []

let executeTests path func = 
    let data = parseFile path
    let testData = createMap (List.tail data) (List.head data)
    let rec execute func testData num = 
        match testData with
        | case :: rest -> 
            sprintf "Test number %i - %s \n" num  (if func case then "Success" else "Failure") + execute func rest (num+1)
        | [] -> "All tests complete"
    execute func testData 0

let andTest (case : Map<string, string>) = 
    And (stringToInt case.["a"]) (stringToInt case.["b"]) = stringToInt case.["out"]

let IncTest (case : Map<string, string>) = 
    case.["in"]
    |> Seq.map (charToInt)
    |> Seq.toArray
    |> Increment
    |> intsToString = case.["out"]