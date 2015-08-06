module Sequential

open ArithmeticLogicUnit
open Utils

type clk =
    | Tick = 0s
    | Tock = 1s
    
let flip = function
    | clk.Tick -> clk.Tock
    | _ -> clk.Tick

[<AbstractClass>]
type Chip() =
    member val inputs: int16 array = Array.empty with get,set
    member val outputs : int16 array = Array.empty with get,set
    abstract member execute: clk -> unit

type DFF() =
    inherit Chip()
    let mutable state = 0s
    override x.execute clk = 
        let pState = state
        match clk with //Only set the state on a tock 
        | clk.Tock -> state <- x.inputs.[0]
        | _ -> x.outputs.[0] <- state

//The DFF (Data Flip Flop)
//I have skipped building this chip from combinatorial chips as it is long winded.

//The premise is that the chip returns in(t-1); That is, the input value from the previous clock cycle.
//A clock cycle is from the beginning of the tick, to the end of a tock.
//The DFF therefore accepts its state on a the tock, but does not expose this on the output pin until the following tick.

//The DFF inherently no needs some form of state, as we need to pass the previous clk cycles value out of the function.
//type DFF() = 
//    let mutable state = 0s
//    member x.execute d clk = 
//        let pState = state
//        match clk with //Only set the state on a tock 
//        | true -> state <- d
//        | _ -> ()
//        pState



//The Set-Reset (SR) Latch - The inputs can be thought of as negated (they need to be set to false to take affect!)
//The 2 output vars (The latch state) are Q and !Q - It is assumed !Q should always be the inverse of Q
//Setting both S and R to false will cause issues with this logic and Q and !Q will be equal!
//As we cannot have a cyclic chip we need to model it with implicit state - This is not technically correct but gets the job done!
//Effectively the state represents the continuous current
//This implementation has inherit propagation delay for an entire cycle but misses the subtlety of the tick-tock 
//type SRLatch() =
//    let mutable state = (false,false)
//    member x.execute s r = 
//        state <- (Nand s (snd state),
//                  Nand (fst state) r)
//        state

type SRLatch() = 
    inherit Chip()
    let mutable state = (0s,0s)
    override x.execute clk = 
        let (s,r) = (x.inputs.[0], x.inputs.[1])
        state <- (Nand s (snd state),
                  Nand (fst state) r)
        x.outputs <- [|fst state; snd state;|]

//Adding the clk into the latch allows us to control when the state is set (Ie -only when the clock is high (true))
type ClockedSRLatch() =
    inherit SRLatch()
    let mutable state = (0s,0s)
    override x.execute clk =
        let (s,r,clk2) = (x.inputs.[0], x.inputs.[1], clk |> int16 )
        x.inputs <- [|Nand s clk2; Nand r clk2|]
        base.execute clk
        
//A master - slave latch configuration
//This adds a delay to the setting of the slave state, allowing the chip to have the entire clock cycle to settle into it's state.
type RsFlipFlop() =
    inherit Chip()
    let mutable state = (0s,0s)
    let master = new ClockedSRLatch()
    let slave = new ClockedSRLatch()
    override x.execute clk = 
        master.inputs <- x.inputs
        master.execute clk
        slave.inputs <- master.outputs
        clk |> flip |> slave.execute 

//Clocked D latch is simply an SR latch with only one input.
//The S input is negated to supply the R input
type ClockedDLatch() =
    inherit Chip()
    let latch = new ClockedSRLatch()
    override x.execute clk =
         latch.inputs <- [|x.inputs.[0]; (Not x.inputs.[0])|]
         latch.execute clk

//The DFF
//It is just an RS Flip Flop with a single input negated to supply both 
//This could also be made form a clockedDLatch and an RS latch instead
type DFlipFlop() =
    inherit Chip()
    let ff = new RsFlipFlop()
    override x.execute clk =
         ff.inputs <- [|x.inputs.[0]; (Not x.inputs.[0])|]
         ff.execute clk

//TODO - Need to make standard interface for state holding chips.

//Stores a single bit.
//The Mux chip acts as a selector on whether to store a new value or keep hold of the old DFF value
type Bit() =
    inherit Chip()
    let dff = new DFF()
    let mutable state = 0s
    override x.execute clk =
        dff.inputs <- [|Mux x.inputs.[0] state x.inputs.[1]|]
        dff.execute clk
        state <- dff.outputs.[0]

//A 16 bit register 
type Register() = 
    inherit Chip()
    let bits = [|for i in 1 .. 16 -> new Bit()|]
    override x.execute clk =
        let inBits = x.inputs.[0] |> toBinary
        bits |> Array.iteri (fun i b -> b.inputs <- [|inBits.[i]; x.inputs.[1]|] 
                                        b.execute clk)
        //TODO -set outputs!


//An 16 bit wide 8 bit size, register array.
//Utilises Mux and DMux to select the correct register to store the value in
type RAM8() =
    inherit Chip()
    let registers = [|for i in 1 .. 8 -> new Register()|]
    override x.execute clk =
        let (inBits, load, address) = (x.inputs.[0],x.inputs.[1],x.inputs.[2])
        let loadArray = DMux8Way load (address |> toBinary)
        registers |> Array.iteri (fun i r -> r.inputs <- [|inBits; loadArray.[i]|]
                                             r.execute clk)
        let state = registers |> Array.map(fun r -> r.outputs)
        //TODO - Fix me!
        Mux8Way16 state.[0] state.[1] state.[2] state.[3] state.[4] state.[5] state.[6] state.[7] address

type RAM64() =
    let ramArray = [|for i in 1 .. 8 -> new RAM8()|]
    member x.execute (inBits: int16 array) clk load (address: int16 array) =
        let ramLoad = DMux8Way load address.[0..2]
        let state = ramArray |> Array.mapi (fun i r -> r.execute inBits clk ramLoad.[i] address.[3..5])
        Mux8Way16 state.[0] state.[1] state.[2] state.[3] state.[4] state.[5] state.[6] state.[7] address

//Beginning to see the pattern.......

type RAM512() =
    let ramArray = [|for i in 1 .. 8 -> new RAM64()|]
    member x.execute (inBits: int16 array) clk load (address: int16 array) =
        let ramLoad = DMux8Way load address.[0..2]
        let state = ramArray |> Array.mapi (fun i r -> r.execute inBits clk ramLoad.[i] address.[3..8])
        Mux8Way16 state.[0] state.[1] state.[2] state.[3] state.[4] state.[5] state.[6] state.[7] address

type RAM4k() =
    let ramArray = [|for i in 1 .. 8 -> new RAM512()|]
    member x.execute (inBits: int16 array) clk load (address: int16 array) =
        let ramLoad = DMux8Way load address.[0..2]
        let state = ramArray |> Array.mapi (fun i r -> r.execute inBits clk ramLoad.[i] address.[3..11])
        Mux8Way16 state.[0] state.[1] state.[2] state.[3] state.[4] state.[5] state.[6] state.[7] address

type RAM16k() =
    let ramArray = [|for i in 1 .. 4 -> new RAM4k()|]
    member x.execute (inBits: int16 array) clk load (address: int16 array) =
        let ramLoad = DMux8Way load address.[0..2]
        let state = ramArray |> Array.mapi (fun i r -> r.execute inBits clk ramLoad.[i] address.[3..13])
        Mux8Way16 state.[0] state.[1] state.[2] state.[3] state.[4] state.[5] state.[6] state.[7] address

type Counter() = 
    let register = new Register()
    member x.execute (inBits: int16 array) clk inc load reset =
        let next = Increment (register.execute inBits clk false) //Increment the current value
        let mux1 = MultiMux load next inBits 
        let mux2 = MultiMux reset mux1 [|for i in 1..16 -> false|]
        let or1 = Or load inc
        let or2 = Or or1 reset
        register.execute mux2 clk or2
        
type CounterPM() =
    let register = new Register()
    member x.execute (inBits: int16 array) clk inc load reset =
        let toSet = 
            match reset, load, inc with
            | true,_,_ -> [|for i in 1..16 -> false|]
            | false,true,_ -> inBits
            | false,false,true -> Increment (register.execute inBits clk false)
            |_,_,_ -> register.execute inBits clk false
        register.execute toSet clk (load || reset || inc)


//Generic implementation!

type RamSize =
    | Bit8 
    | Bit64 
    | Bit512
    | KB4   
    | KB16  

let getSize = function 
    | Bit8   -> 8
    | Bit64  -> 64
    | Bit512 -> 512
    | KB4    -> 4096
    | KB16   -> 16384
    
    
//More F# approach
type RAM(size) = 
    let regArray = [|for i in 1 .. size |> getSize -> new Register()|]
    member x.execute (inBits: bool array) clk load (address: bool array) =
        let index = address |> Array.map boolToInt
                            |> toDecimal 16
        regArray.[index].execute inBits clk load //Need to handle index out of range

    
type TestHarness = 
    {
        inputs:bool array; 
        outputs: bool array option;
        chip: bool array -> bool array 
    }

let cycle iterations (harness:TestHarness) =
    let rec doCycle i state = 
        let result = {state with outputs = Some (harness.chip state.inputs) }
        printfn "%A" result.outputs
        if i > 1
        then doCycle (i-1) result
        else result 
    doCycle iterations harness

let setInputs ins harness = 
    {harness with inputs = ins;}

//let testLatch (i: bool array) =
//    let (a,b) = l.execute i.[0] i.[1]
//    [|a;b;|]