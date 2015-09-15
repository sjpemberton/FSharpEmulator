
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
    cycle 10 th

