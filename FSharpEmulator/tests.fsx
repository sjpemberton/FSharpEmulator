
#load "ArithmeticLogicUnit.fs"
#load "Utils.fs"
#load "Sequential.fs"

open ArithmeticLogicUnit
open Sequential
open Utils

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


module CombinatorialExpressions = 

    let OrM a b = a || b
    let AndM a b = a && b
    let Not a = not a
    let Nand a b = 
        match a, b with
        | true, true -> false
        | _, _ -> true

    type fakeLatch() = 
        let mutable state = [|false; false|]
        member x.exec clk a b =
            state <- [|Nand a state.[1];
                       Nand b state.[0]|]
            state 

    type SimulationState =  
        | Valid of int16 array
        | Illegal of string

    let execute (inputs: int16 array) (gate:int16 -> int16 -> int16)  =
        if inputs.Length < 2
        then Illegal "Insufficient inputs provided"
        else Valid [|(gate inputs.[0] inputs.[1])|]

    let combine a b =
        Valid (Array.append a b)

    type CircuitBuilder() =
        member this.Bind(m, f) = 
            match m with
            | Illegal _ -> m
            | Valid a -> 
                printfn "\State is Valid: %A" a
                f a
        member this.Return(x) = Valid x

    let circuit = new CircuitBuilder()

    let workflow = 
        circuit 
            {
            let! a = And |> execute [|0s; 1s|]
            let! b = Or |> execute [|0s; 1s|]
            let! c = combine a b
//            let! b = a |> execute w
//            let! c = b |> execute z
            return c
            }    

let TestCounter = 
    let harness = {inputs = [|0s; 0s; 1s; 0s;|]; outputs = Array.empty; chips = [|new Counter()|]}
    harness
    |> cycle 3  3

    // test
    //let good = workflow And Or Or
    //let bad = workflow 12 3 0 1

    //Need to do:

    //let someFunc a b =  
    //Parralel Nand a a, Nand b b
    //|> Nand