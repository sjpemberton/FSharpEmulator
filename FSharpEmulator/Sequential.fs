module Sequential

open ArithmeticLogicUnit

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


//We could add the clk into the execute, but this is not needed at this stage.
//I've included it for completeness/correctness. This adds that extra level of propagation delay.
//
type SRLatchClocked() =
    let mutable state = (false,false)
    member x.execute s r clk =
        let pState = state
        match clk with //Only set the state on a tock 
        | true -> state <- 
                  (Nand s (snd state),
                   Nand (fst state) r)
        | _ -> ()
        pState 

     