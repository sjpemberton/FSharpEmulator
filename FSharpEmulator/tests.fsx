
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

type testLatch() = 
    inherit Chip()
    let mutable state = (None, None)
    override x.doWork clk inputs = 
        let (s,r) = (Nand inputs.[0] (clk |> int16), Nand (clk |> int16) inputs.[1])
        match state with 
        | Some x, Some y ->
            state <- (Some (Nand s y),
                      Some (Nand x r))
        | _,_ -> state <- (Some 0s, Some 0s)
        [|(fst state).Value; (snd state).Value;|]

let tl = new testLatch()
tl.execute clk.Tick [|0s;1s|]
tl.execute clk.Tock [|0s;1s|]

let latch = new SRLatch()
let i = [|Nand 0s 0s; Nand 0s 1s|];;
latch.execute clk.Tick i
let i2 = [|Nand 0s 1s; Nand 1s 1s|];;
latch.execute clk.Tock i2

let master = new ClockedSRLatch()
let slave = new ClockedSRLatch()

let a = master.execute clk.Tick [|0s;01s|]
let b = slave.execute clk.Tock a

let x = master.execute clk.Tock [|0s;1s|]
let y = slave.execute clk.Tick x

let v = master.execute clk.Tick [|0s;1s|]
let u = slave.execute clk.Tock v

let rsff = new RsFlipFlop()
let a1 = rsff.execute clk.Tick [|0s;1s|]
let a2 = rsff.execute clk.Tock [|0s;1s|]
let a3 = rsff.execute clk.Tick [|0s;1s|]
let a4 = rsff.execute clk.Tock [|0s;1s|]