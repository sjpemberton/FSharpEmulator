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

let Mux4Way16 a b c d (sel:bool array) = 
    let m1 = MultiMux sel.[0] a b 
    let m2 = MultiMux sel.[0] c d
    MultiMux sel.[1] m1 m2 

let Mux8Way16 a b c d e f g h (sel:bool array) =
    let m1 = Mux4Way16 a b c d sel.[0..1]
    let m2 = Mux4Way16 e f g h sel.[0..1]
    MultiMux sel.[2] m1 m2 

let DMux4Way x (sel:bool array) = 
    let (d1,d2) = DMux x sel.[1]
    let (a,b) = DMux d1 sel.[0]
    let (c,d) =  DMux d2 sel.[0]
    (a,b,c,d)

let DMux8Way x (sel:bool array) = 
    let (d1,d2) = DMux x sel.[2]
    let (a,b,c,d) = DMux4Way d1 sel.[0..1]
    let (e,f,g,h) = DMux4Way d2 sel.[0..1]
    (a,b,c,d,e,f,g,h)