module Sequential

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
    