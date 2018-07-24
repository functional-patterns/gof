//
// STATE
//
// Allow an object to alter its behavior when its internal state changes. The object will appear to
// change its class.
//

//
// NOTE
//
// By nature functional program is immutable. State has to come from the outside of the pure
// functional core logic.
//
// For example infinite sequences may present incoming data. A fold function transforms sequence of
// elements and initial state to a final state.
//

//
// Seems that State monad is just another presentation for fold. In State monad the folder
// function's signature is a -> State s () where as it is a -> b -> in fold.
//

// let apply fn (state : 'State) (element : 'Element) : ('Result * 'State) =
//     fn state element


// type Tool =  Pan | Zoom | Rotate

// type Drawing = Shape list
// and Shape = Circle | Rectangle | Line




// let apply (f : ('a -> 'b) option) (a : 'a option) : 'b option =
//     match f, a with
//         | None, _ -> None
//         | _, None -> None
//         | Some f', Some a' -> Some (f' a')

// let pure = Some

// let (<*>) = apply




// pure (+) <*> Some 5 <*> None




// let foo x = Some (x + 1)
// let divTenBy x = if x = 0 then None else Some (10 / x)

// let bind (f : int -> int option) (mA : int option) : int option =
//     match mA with
//         | Some a -> f a
//         | _ -> None

// let (>>=) x y = bind y x

// (Some 0) >>= foo >>= foo >>= divTenBy

// Some 1 >>= (fun x ->
// Some 1 >>= (fun y ->
// Some 5 >>= (fun z ->
// Some (x + y + z)))) >>= (fun a ->
// Some 5 >>= (fun b ->
// Some (a + b)))



let (>>=) (a : 'a list) (f : 'a -> 'b list) : 'b list =
    a |> List.map f |> List.concat
    //List.concat (List.map f a)


[1; 2; 3] >>= (fun x -> [x; x * x])


let fmap (f : 'a -> 'b) (a : 'a list) : 'b list =
    List.map f a

let (<|>) a b = fmap a b

let pure x = [x]

let rec apply (fl : ('a -> 'b) list) (al : 'a list) : 'b list =
    match fl, al with
        | [], _ ->
            []
        | _, [] ->
            []
        | f::fl, al ->
            fmap f al @ apply fl al
      
let (<*>) = apply



let inc x = 1 + x
let pow x = x * x
let dec x = x - 1

apply [inc; dec] [1; 2; 3]

[inc; dec; pow] <*> [10; 20; 30; 40]



let add x y = x + y
let mul x y = x * y
let sub x y = x - y


[add; mul; sub] <*> [1; 2; 3] <*> []













[1;2;3] :: [[5;6;7]; [10;11]] |> List.concat




