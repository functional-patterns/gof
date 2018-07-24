




let bind (aM : 'a option) (fM : 'a -> 'b option) : 'b option =
    match aM with
        | Some a -> fM a
        | _ -> None

let (>>=) = bind

let pure (a : 'a) : 'a option = Some a


let divTenBy x =
    match x with
        | 0 -> None
        | x -> Some (10 / x)

let inc x = x + 1

divTenBy 1

let doNothing = fun _ -> None

Some 5 >>= doNothing >>= divTenBy

Some 5 >>= (fun x ->
Some 7 >>= (fun y ->
Some 0 >>= (fun y ->
Some (x + y))))



type StateMonadBuilder() =
    member x.Bind(param) =
        let aM = fst param
        let fM = snd param
        match aM with
            | Some a -> fM a
            | _ -> None

    member x.Return(a : 'a) : 'a option =
        Some a


let builder = new StateMonadBuilder()

builder {
    let! x = divTenBy 2
    let! y = Some 7
    return (x + y + 1)
}



// (m a) -> (a -> m b) -> m b
module Foo =
    
    // ('a -> ('a * 's)) -> ()
    let bind 
        
