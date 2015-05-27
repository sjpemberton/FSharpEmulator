module ArithmeticLogicUnit

let Nand a b = 
    match a, b with
    | true, true -> false
    | _, _ -> true


let NotPM a = function
    | true -> false
    | false -> true

let Not a = Nand a a

let AndPM a b = 
    match a, b with
    | true, true -> true
    | _, _ -> false

let And a b = Nand a b |> Not

let OrPM a b =
    match a, b with
    | true, false -> true
    | false, true -> true
    | true, true -> true
    | false, false -> false 

let Or a b =
    Nand (Nand a a) (Nand b b)

let XorPM a b =
    match a, b with
    | true, false -> true
    | false, true -> true
    | _, _ -> false

let Xor a b = 
    Or (And a (Not b)) (And (Not a) b) 

let MuxPM a b sel =
    match sel with
    | false -> a
    | _ -> b

let Mux a b sel =
    Nand (Nand a sel) (Nand (Not sel) b)    

let DMuxPM x sel =
    match sel with
    | false -> (x,false)
    | _ -> (false,x)

let Dmux x sel =
    (And x (Not x), And x sel)