module Sequential

open ArithmeticLogicUnit
open Utils

type clk =
    | Tick
    | Tock

[<AbstractClass>]
type Chip() =
    member x.inputs: int array = Array.empty
    member x.outputs: int array = Array.empty
    abstract member execute: clk -> unit

type testDff() =
    inherit Chip()
    let mutable state = false
    override this.execute clk = 
        let pState = state
        match clk with //Only set the state on a tock 
        | Tock -> state <- this.inputs.[1] |> intToBool
        | _ -> ()

//The DFF (Data Flip Flop)
//I have skipped building this chip from combinatorial chips as it is long winded.

//The premise is that the chip returns in(t-1); That is, the input value from the previous clock cycle.
//A clock cycle is from the beginning of the tick, to the end of a tock.
//The DFF therefore accepts its state on a the tock, but does not expose this on the output pin until the following tick.

//The DFF inherently no needs some form of state, as we need to pass the previous clk cycles value out of the function.
type DFF() = 
    let mutable state = false
    member x.execute d clk = 
        let pState = state
        match clk with //Only set the state on a tock 
        | true -> state <- d
        | _ -> ()
        pState



//The Set-Reset (SR) Latch - The inputs can be thought of as negated (they need to be set to false to take affect!)
//The 2 output vars (The latch state) are Q and !Q - It is assumed !Q should always be the inverse of Q
//Setting both S and R to false will cause issues with this logic and Q and !Q will be equal!
//As we cannot have a cyclic chip we need to model it with implicit state - This is not technically correct bug gets the job done!
//Effectively the state represents the continuous current
//This implementation has inherit propagation delay for an entire cycle but misses the subtlety of the tick-tock 
type SRLatch() =
    let mutable state = (false,false)
    member x.execute s r = 
        state <- (Nand s (snd state),
                  Nand (fst state) r)
        state


//Adding the clk into the latch allows us to control when the state is set (Ie -only when the clock is high (true))
type ClockedSRLatch() =
    let mutable state = (false,false)
    member x.execute s r clk =
        let (ns, nr) = (Nand s clk, Nand r clk)
        state <- (Nand ns (snd state),
                  Nand (fst state) nr)
        state

//A master - slave latch configuration
//This adds a delay to the setting of the slave state, allowing the chip to have the entire clock cycle to settle into it's state.
type RsFlipFlop() =
    let mutable state = (false,false)
    let master = new ClockedSRLatch()
    let slave = new ClockedSRLatch()
    member x.execute s r clk = 
        let (a,b) = master.execute s r clk
        slave.execute a b (Not clk)

//Clocked D latch is simply an SR latch with only one input.
//The S input is negated to supply the R input
type ClockedDLatch() =
    let latch = new ClockedSRLatch()
    member x.execute d clk =
        latch.execute d (Not d) clk

//The DFF
//It is just an RS Flip Flop with a single input negated to supply both 
type DFlipFlop() =
    let ff = new RsFlipFlop()
    member x.execute d clk =
        ff.execute d (Not d) clk

//TODO - Need to make standard interface for state holding chips.

//Stores a single bit.
//The Mux chip acts as a selector on whether to store a new value or keep hold of the old DFF value
type Bit() =
    let dff = new DFF()
    let mutable state = false
    member x.execute d clk load =
        state <- dff.execute (Mux d state load) clk
        state

//A 16 bit register - We could of course parameterise the constructer with the array size
type Register() = 
    let bits = [|for i in 1 .. 16 -> new Bit()|]
    member x.execute (inBits: bool array) clk load =
         bits |> Array.mapi (fun i b -> b.execute inBits.[i] clk load)

//An 16 bit wide 8 bit size, register array.
//Utilises Mux and DMux to select the correct register to store the value in
type RAM8() =
    let registers = [|for i in 1 .. 8 -> new Register()|]
    member x.execute (inBits: bool array) clk load (address: bool array) = 
        let loadArray = DMux8Way load address
        let state = registers |> Array.mapi (fun i r -> r.execute inBits clk loadArray.[i])
        Mux8Way16 state.[0] state.[1] state.[2] state.[3] state.[4] state.[5] state.[6] state.[7] address

type RAM64() =
    let ramArray = [|for i in 1 .. 8 -> new RAM8()|]
    member x.execute (inBits: bool array) clk load (address: bool array) =
        let ramLoad = DMux8Way load address.[0..2]
        let state = ramArray |> Array.mapi (fun i r -> r.execute inBits clk ramLoad.[i] address.[3..5])
        Mux8Way16 state.[0] state.[1] state.[2] state.[3] state.[4] state.[5] state.[6] state.[7] address

//Beginning to see the pattern.......

type RAM512() =
    let ramArray = [|for i in 1 .. 8 -> new RAM64()|]
    member x.execute (inBits: bool array) clk load (address: bool array) =
        let ramLoad = DMux8Way load address.[0..2]
        let state = ramArray |> Array.mapi (fun i r -> r.execute inBits clk ramLoad.[i] address.[3..8])
        Mux8Way16 state.[0] state.[1] state.[2] state.[3] state.[4] state.[5] state.[6] state.[7] address

type RAM4k() =
    let ramArray = [|for i in 1 .. 8 -> new RAM512()|]
    member x.execute (inBits: bool array) clk load (address: bool array) =
        let ramLoad = DMux8Way load address.[0..2]
        let state = ramArray |> Array.mapi (fun i r -> r.execute inBits clk ramLoad.[i] address.[3..11])
        Mux8Way16 state.[0] state.[1] state.[2] state.[3] state.[4] state.[5] state.[6] state.[7] address

type RAM16k() =
    let ramArray = [|for i in 1 .. 4 -> new RAM4k()|]
    member x.execute (inBits: bool array) clk load (address: bool array) =
        let ramLoad = DMux8Way load address.[0..2]
        let state = ramArray |> Array.mapi (fun i r -> r.execute inBits clk ramLoad.[i] address.[3..13])
        Mux8Way16 state.[0] state.[1] state.[2] state.[3] state.[4] state.[5] state.[6] state.[7] address

//type Counter() = 
//    let register = new Register()
//    member x.execute (inBits: bool array) inc load reset =

        

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