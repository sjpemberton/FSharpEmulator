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

let DMux x sel =
    (And x (Not x), And x sel)

let MultiNot input = 
    input |> Seq.map Not

let MultiAnd a b = 
    Seq.zip a b 
    |> Seq.map (fun (a,b) -> And a b)

let MultiOr a b =
    Seq.zip a b 
    |> Seq.map (fun (a,b) -> Or a b)

let MultiMux a b sel = 
    Seq.zip a b 
    |> Seq.map (fun (a,b) -> Mux a b sel)

let MultiDMux input sel = 
    input
    |> Seq.map (DMux sel)
    
let MultiWayOr input =
    input
    |> Seq.reduce Or

let Mux4Way16 a b c d sel = 
    let m1 = MultiMux a b (fst sel)
    let m2 = MultiMux c d (fst sel)
    MultiMux m1 m2 (snd sel)
