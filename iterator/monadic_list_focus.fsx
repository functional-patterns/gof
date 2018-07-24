

type Focus<'a> = 'a list * 'a list

let forward (xs, fs) =
    match xs, fs with
        | [], fs -> (xs, fs)
        | x::xs, fs -> (xs, x::fs)

let backward (xs, fs) =
    match xs, fs with
        | xs, [] -> (xs, fs)
        | xs, f::fs -> (f::xs, fs)

let modify a (xs, fs) =
    match xs, fs with
        | [], fs -> (xs, fs)
        | _::tail, fs -> (a::tail, fs)



type IterableList<'a> = 'a list * 'a list


let ls = IterableList ([ 1 .. 10 ], [])



let (>->) ls = forward ls
let (<-<) ls = backward ls


ls |> forward |> forward |> modify 20 |> backward |> backward |> forward |> modify -1

([1; 2; 3; 4; 5; 6], []) |> forward |> forward |> forward |> modify 0 
