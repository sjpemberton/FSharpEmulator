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

let Mux sel a b =
    Nand (Nand a sel) (Nand (Not sel) b)    

let DMuxPM x sel =
    match sel with
    | false -> (x,false)
    | _ -> (false,x)

let DMux x sel =
    (And x (Not x), And x sel)


let unaryArray gate bits =
    bits |> Array.map gate

let binaryArray gate aBits bBits =
    Array.zip aBits bBits 
    |> unaryArray gate

//let tertiaryArray func a b c =
//    Array.zip a b 
//    |> unaryArray func

let MultiNot = unaryArray Not

let MultiAnd = binaryArray (fun (a,b) -> And a b)

let MultiOr = binaryArray (fun (a,b) -> Or a b)

let MultiMux sel = binaryArray (fun (a,b) -> Mux a b sel)

let MultiDMux sel = unaryArray (DMux sel)
    
let MultiWayOr bits = 
    bits |> Array.reduce Or

let Mux4Way16 a b c d sel = 
    let m1 = MultiMux (fst sel) a b 
    let m2 = MultiMux (fst sel) c d 
    MultiMux (snd sel) m1 m2 
