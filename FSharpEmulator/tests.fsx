
#load "ArithmeticLogicUnit.fs"
#load "Utils.fs"
#load "Sequential.fs"

open ArithmeticLogicUnit
open Sequential

let testLatch = 
    let harness = {inputs = [|1s;0s|]; outputs = Array.empty; chips = [|new SRLatch()|]}
    harness 
    |> cycle 3 
    |> setInputs [|1s;1s|]
    |> cycle 2
    |> setInputs [|0s;1s|]
    |> cycle 3


let testClockedLatch = 
    let harness = {inputs = [|0s;1s|]; outputs = Array.empty; chips = [|new ClockedSRLatch()|]}
    harness
    |> cycle 3 
    |> setInputs [|0s;0s|]
    |> cycle 2
    |> setInputs [|1s;0s|]
    |> cycle 3

let testSRFlipFlop = 
    let harness = {inputs = [|0s;1s|]; outputs = Array.empty; chips = [|new RsFlipFlop()|]}
    harness
    |> cycle 3 
    |> setInputs [|0s;0s|]
    |> cycle 2
    |> setInputs [|1s;0s|]
    |> cycle 3

let testClockedDLatch = 
    let harness = {inputs = [|0s;1s|]; outputs = Array.empty; chips = [|new ClockedDLatch()|]}
    harness
    |> cycle 3 
    |> setInputs [|0s;0s|]
    |> cycle 2
    |> setInputs [|1s;0s|]
    |> cycle 3

let testDFF = 
    let harness = {inputs = [|0s;1s|]; outputs = Array.empty; chips = [|new DFlipFlop()|]}
    harness
    |> cycle 3 
    |> setInputs [|0s;0s|]
    |> cycle 2
    |> setInputs [|1s;0s|]
    |> cycle 3

