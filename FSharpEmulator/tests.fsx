
#load "ArithmeticLogicUnit.fs"
#load "Utils.fs"
#load "Sequential.fs"

open ArithmeticLogicUnit
open Sequential

let testLatchPD = 
    let harness = {inputs = [|1s;0s|]; outputs = Array.empty; chips = [|new SRLatch()|]}
    harness
    |> iterate 3 clk.Tick

let testLatch = 
    let harness = {inputs = [|1s;0s|]; outputs = Array.empty; chips = [|new SRLatch()|]}
    harness 
    |> cycle 3 3 
    |> setInputs [|1s;1s|]
    |> cycle 2 3
    |> setInputs [|0s;1s|]
    |> cycle 3 3


let testClockedLatch = 
    let harness = {inputs = [|0s;1s|]; outputs = Array.empty; chips = [|new ClockedSRLatch()|]}
    harness
    |> iterate 3 clk.Tock
    |> setInputs [|1s;0s|]
    |> iterate 3 clk.Tick
    |> iterate 3 clk.Tock

let testClockedLatchCycle = 
    let harness = {inputs = [|0s;1s|]; outputs = Array.empty; chips = [|new ClockedSRLatch()|]}
    harness
    |> cycle 3 3
    |> setInputs [|0s;0s|]
    |> cycle 2 3
    |> setInputs [|1s;0s|]
    |> cycle 3 3

let testSRFlipFlop = 
    let harness = {inputs = [|0s;1s|]; outputs = Array.empty; chips = [|new RsFlipFlop()|]}
    harness
    |> iterate 3 clk.Tock
    |> iterate 3 clk.Tick
    |> setInputs [|1s;0s|]
    |> iterate 3 clk.Tock
    |> iterate 3 clk.Tick

let testSRFlipFlopCycle = 
    let harness = {inputs = [|0s;1s|]; outputs = Array.empty; chips = [|new RsFlipFlop()|]}
    harness
    |> cycle 2 4
    |> setInputs [|0s;0s|]
    |> cycle 2 4
    |> setInputs [|1s;0s|]
    |> cycle 2 4

let testClockedDLatch = 
    let harness = {inputs = [|0s;1s|]; outputs = Array.empty; chips = [|new ClockedDLatch()|]}
    harness
    |> cycle 3  3
    |> setInputs [|0s;0s|]
    |> cycle 2 3
    |> setInputs [|1s;0s|]
    |> cycle 3 3

let testDFF = 
    let harness = {inputs = [|0s|]; outputs = Array.empty; chips = [|new DFF()|]}
    harness
    |> cycle 2 4
    |> setInputs [|0s|]
    |> cycle 2 4
    |> setInputs [|1s|]
    |> cycle 2 4

