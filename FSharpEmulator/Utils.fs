﻿module Utils

open ArithmeticLogicUnit
open System.IO
open System

let stringToInt = function 
    | "1" -> 1
    | _ -> 0

let charToInt = function 
    | '1' -> 1
    | _ -> 0

let intToBool = function 
    | 1 -> true
    | _ -> false

let boolToInt = function 
    | true -> 1
    | _ -> 0

let intToString = function 
    | 1 -> "1"
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
        | _ when i > 0 -> (i % 2) :: (convert (i / 2) acc)
        | _ -> acc
    convert i [] |> List.rev

let toDecimal b = 
    let rec convert b i acc = 
        match b with
        | h :: t -> ((float h) * 2.0 ** i) + (convert t (i + 1.0) acc)
        | [] -> acc
    convert (b |> List.rev) 0.0 0.0 |> int

let convertToTwosCompliment b = 
    let rec convert b acc = 
        match b with
        | h :: t -> 
            match h with
            | 1 -> 0 :: convert t acc
            | _ -> 1 :: convert t acc
        | [] -> acc
    convert b []

let padBits length (bits : int list) = 
    let pad = length - bits.Length
    [ for i in 1..pad -> 0 ]
    @ bits

//Doesn't handle overflow
let toTwosCompliment i b = 
    let convertToBinary = 
        function 
        | _ when i < 0 -> 
            abs i
            |> toBinary
            |> padBits b
            |> convertToTwosCompliment
            |> List.toArray
            |> Array.map intToBool
            |> Incrementer
            |> Array.map boolToInt
            |> Array.toList
        | _ -> 
            i
            |> toBinary
            |> padBits b
    convertToBinary i

let toDecimalFromTC (binary : int list) =
    let result = binary |> toDecimal
    match binary.[0] with
    | 0 -> result
    | _ -> -result

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
    And (stringToBool case.["a"]) (stringToBool case.["b"]) = stringToBool case.["out"]

let IncTest (case : Map<string, string>) = 
    case.["in"]
    |> Seq.map (charToInt >> intToBool)
    |> Seq.toArray
    |> Incrementer
    |> boolsToString = case.["out"]