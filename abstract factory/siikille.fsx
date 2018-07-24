


let div b a =
    if b = 0 then None else Some (a / b)

// 120 / 10 / 6 = 12 / 6 = 2

120
|> div 10
|> (fun t ->
        match t with
            | Some x -> div 6 x
            | _ -> None)












let bind' (f : int -> int option) (m : int option) : int option =
    match m with
        | Some x -> f x
        | _ -> None

let return' a = Some a


return' 120
|> bind' (div 0)
|> bind' (div 6)



let (>>=) (m : int option) (f : int -> int option) : int option =
    match m with
        | Some x -> f x
        | _ -> None

return' 120
>>= (div 10)
>>= (div 6)
































let div' a b =
    if b = 0 then None else Some (a / b)



type DivBuilder() =
    member this.Bind(m : int option, f : int -> int option) =
        printfn "temporary value %A" m
        match m with
            | Some x -> f x
            | _ -> None

    member this.Return(x : int) : int option =
        Some x




let divBuilder = new DivBuilder()

let workflow a b c =
    divBuilder {
        let x = a
        let! y = div' x b
        let! z = div' y c
        return z
    }

workflow 120 10 6






module Option =

    // The apply function for Options
    let apply fOpt xOpt = 
        match fOpt,xOpt with
        | Some f, Some x -> Some (f x)
        | _ -> None

    let applys (f : ('a -> 'b) option) (m : 'a option) : 'b option =
        match f, m with
            | Some f, Some x -> Some (f x)
            | _ -> None

Option.apply (Some (fun t -> float t + 1.0)) (Some 1)
Option.apply (Option<int -> int>.None) (Some 1)

let foo : (int -> int -> float) option =
    Option<int -> int -> float>.None

Option.apply foo (Some 5)
|> Option.apply

let (<!>) = Option.apply



foo <!> (Some 5)


let add a b = a + b

Option.map2 add

let ret f = Some f

ret add
|> Option.apply
