




type Result<'a> =
    | Success of 'a
    | Failure of string list

type Customer = {
    CustomerId : int
    Email : string
}

let createCustomerId i =
    if i < 0 then Failure ["customer id has to be positive"] else Success i

let createEmail (s : string) =
    if s.Contains("@")
    then Success s
    else Failure ["email must contain @"]

let createCustomer id email =
    { CustomerId = id; Email = email }

let bind (f : 'a -> Result<'b>) (m : Result<'a>) : Result<'b> =
    match m with
        | Success a ->
            f a
        | Failure errors ->
            Result<'b>.Failure errors

let retn (a : 'a) =
    Success a

let map (f : 'a -> 'b) (aR : Result<'a>) : Result<'b> =
    printfn "MAPPING"
    match aR with
        | Success a ->
            Success (f a)
        | Failure errors ->
            Result<'b>.Failure errors

let apply (fR : Result<'a -> 'b>) (aR : Result<'a>) : Result<'b> =
    printfn "APPLYING"
    match fR, aR with
        | Success f, Success a
            -> Success (f a)
        | Failure fErrors, Failure aErrors ->
            Result<'b>.Failure (fErrors @ aErrors)
        | _, Failure errors ->
            Result<'b>.Failure errors
        | Failure errors, _ ->
            Result<'b>.Failure errors

let (<!>) = map
let (<*>) = apply

let createCustomerResultA id email =
    let idR = createCustomerId id
    let emailR = createEmail email

    createCustomer <!> idR <*> emailR

let badId = -1
let goodId = 1
let badEmail = "foobar"
let goodEmail = "foo@bar"


createCustomerResultA badId badEmail
createCustomerResultA badId goodEmail
createCustomerResultA goodId badEmail
createCustomerResultA goodId goodEmail


let (>>=) a f = bind f a


let createCustomerResultM id email =
    createCustomerId id >>= (fun _ ->
    createEmail email >>= (fun _ ->
    let customer = createCustomer id email
    Success customer
    ))

createCustomerResultM badId badEmail
createCustomerResultM badId goodEmail
createCustomerResultM goodId badEmail
createCustomerResultM goodId goodEmail


let rec map' f ls =
  match ls with
    | [] -> []
    | h::t -> (f h)::(map' f t)


map' (fun t -> 2 * t) [ 1; 2; 5; ]


Seq.initInfinite (fun i ->
    match i with
        | 0 -> 0
        | 1 -> 1
    )

let fibonacci = 
    
    let folder s _ = (snd s, fst s + snd s)

    Seq.initInfinite (fun _ -> 0)
    |> Seq.scan folder (0, 1)
    |> Seq.map fst

Seq.take 10 fibonacci |> List.ofSeq



let foos = [1; 2; 3; 4]

let matchaa() = 
    match foos with
        | h::_ -> printfn "%A" h
        | _ -> printfn "Nothing"


matchaa()

