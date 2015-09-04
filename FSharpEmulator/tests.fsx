
#load "ArithmeticLogicUnit.fs"
#load "Utils.fs"
#load "Sequential.fs"

open ArithmeticLogicUnit
open Sequential

let testLatch = 
    let th = {inputs = [|1s;0s|]; outputs = Array.empty; chips = [|new SRLatch()|]}
    cycle 5 th

let testClockedLatch = 
    let th = {inputs = [|0s;1s|]; outputs = Array.empty; chips = [|new ClockedSRLatch()|]}
    cycle 5 th

let testSRFlipFlop = 
    let th = {inputs = [|0s;1s|]; outputs = Array.empty; chips = [|new RsFlipFlop()|]}
    cycle 5 th

let master = new ClockedSRLatch()
let slave = new ClockedSRLatch()
 

let a = master.execute clk.Tick [|0s;1s|]
let b = slave.execute clk.Tock a

let x = master.execute clk.Tick [|0s;1s|]
let y = slave.execute clk.Tock x

let v = master.execute clk.Tick [|0s;1s|]
let u = slave.execute clk.Tock v

let rsff = new RsFlipFlop()
let a1 = rsff.execute clk.Tick [|0s;1s|]
let a2 = rsff.execute clk.Tick [|0s;1s|]
let a3 = rsff.execute clk.Tick [|0s;1s|]
let a4 = rsff.execute clk.Tick [|0s;1s|]