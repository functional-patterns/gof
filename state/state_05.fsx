




let bind (mA : 'a option) (f : 'a -> 'b option) : ('b option) =
    match mA with
        | Some a -> f a
        | _ -> None

let (>>=) = bind

let pure = Some

let divTenBy a =
    match a with
        | 0 -> None
        | _ -> Some (10 / a)

let div a b =
    match b with
        | 0 -> None
        | _ -> Some (a / b)



Some 5 >>= divTenBy

Some 1 >>= divTenBy >>= divTenBy



Some 5 >>= (fun x ->
divTenBy x >>= (fun y ->
Some 2 >>= (fun z ->
pure (x + y / z))))


5 + (10 / 5) / 2



