module ArithmeticLogicUnit

module PatternMatched =

    let Nand a b = 
        match a, b with
        | true, true -> false
        | _, _ -> true

    let Not a = function
        | true -> false
        | false -> true

    let And a b = 
        match a, b with
        | true, true -> true
        | _, _ -> false

    let Or a b =
        match a, b with
        | true, _ -> true
        | _, true -> true
        | _, _ -> false 

    let Xor a b =
        match a, b with
        | true, false -> true
        | false, true -> true
        | _, _ -> false

    let Mux sel a b  =
        match sel with
        | false -> a
        | _ -> b

    let DMux sel x =
        match sel with
        | false -> (x,false)
        | _ -> (false,x)

let Nand a b = 
    match a, b with
    | true, true -> false
    | _, _ -> true

let Not a = Nand a a

let And a b = Nand a b |> Not

let Or a b =
    Nand (Nand a a) (Nand b b)

let Xor a b = 
    Or (And a (Not b)) (And (Not a) b) 

let XNOR a b =
    Nand (Or a b) (Nand a b) 

//Basic selector
let Mux sel a b =
    Nand (Nand a sel) (Nand (Not sel) b)    

let DMux x sel =
    (And x (Not sel), And x sel)

let unaryArray gate bits =
    bits |> Array.map gate

let binaryArray gate aBits bBits =
    Array.zip aBits bBits 
    |> unaryArray (fun (a,b) -> gate a b)

let MultiNot = unaryArray Not

let MultiAnd = binaryArray And

let MultiOr = binaryArray Or

let MultiMux sel = 
    Mux sel
    |> binaryArray

let MultiDMux sel = 
    DMux sel
    |> unaryArray
    
let MultiWayOr bits = 
    bits |> Array.reduce Or

let MultiXNOR = binaryArray XNOR

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
    let (c,d) = DMux d2 sel.[0]
    [|a;b;c;d|]

let DMux8Way x (sel:bool array) = 
    let (d1,d2) = DMux x sel.[2]
    DMux4Way d1 sel.[0..1]
    |> Array.append (DMux4Way d2 sel.[0..1])

let HalfAdder a b = 
    let sum = Xor a b
    let carry = And a b
    (sum,carry)

let FullAdder a b c = 
    let (s1,c1) = HalfAdder a b
    let (sum,c2) = HalfAdder s1 c
    (sum, Or c1 c2)

//Ripple Carry Adder Implementation
let Adder aBits bBits =
    let rec addBits aBits bBits carry accu = 
        match aBits, bBits with
        | aHead :: aTail, bHead :: bTail -> 
            let (sum,c) = FullAdder aHead bHead carry
            addBits aTail bTail c (sum :: accu)
        | [],_
        | _,[] -> accu
    addBits (aBits |> Array.rev |> Array.toList) (bBits |> Array.rev |> Array.toList) false List.empty
    |> List.toArray

//In plus one
let Increment aBits = Adder aBits [| for i in 1 .. 16 -> match i with | 16 -> true | _ -> false |]

let ALU xBits yBits nx zx ny zy f no = 
    //handle x    
    let ox1 = MultiMux zx xBits [|for i in 1..16 -> false |]  //Zero all X bits if zx
    let nox1 = MultiNot ox1 //What would this be if negated
    let ox2 = MultiMux nx ox1 nox1 //Select based on nx

    //handle y
    let oy1 = MultiMux zy yBits [|for i in 1..16 -> false |]  //Zero all X bits if zy
    let noy1 = MultiNot oy1 //What would this be if negated
    let oy2 = MultiMux ny oy1 noy1 //Select based on ny

    //handle & / +
    let o3 = MultiAnd ox2 oy2 //an and would be
    let o4 = Adder ox2 oy2 //addition would be

    //Output
    let o5 = MultiMux f o3 o4 //Choose and or addition
    let no5 = MultiNot o5 //Negated out would be
    let out = MultiMux no o5 no5 //Choose to negate or not

    let zr = Not (MultiWayOr out)
    let ng = MultiWayOr (MultiAnd out [|for i in 1..16 -> match i with | 16 -> true | _ -> false|] )
    
    (out, zr, ng)

