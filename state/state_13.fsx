// fromStoAandS :: Int -> (String,Int)
// fromStoAandS c | c `mod` 5 == 0 = ("foo",c+1)
//                | otherwise = ("bar",c+1)


let fromStateStoAandS c =
    if c % 5 = 0 then
        ("foo", c + 1)
    else
        ("bar", c + 1) 


let stateIntString = fromStateStoAandS

let runState = id

runState stateIntString 1


let pure a = fun s -> (a, s)


let bind (m : 's -> 'a * 's) (k : 'a -> 's -> 'b * 's) : 's -> 'b * 's =
    fun s ->
        let (a, s') = runState m s
        (k a) s'

// push :: State [a] ()
// push a = State $ \as -> (() , a:as)
let push (a : 'a) : ('s -> unit * 'a list) =
     fun al -> ((), a :: al)
    
let pop (state : 'a * 's list) =
     match state with
         | _, head::tail -> (head, tail)



type StateMonadBuilder() =
    member x.Return(a) =
        fun s -> (a, s)
    member x.ReturnFrom(a) = a
    member x.Bind((m, k)) = 
        fun s ->
            let (a, s') = runState m s
            (k a) s'
    member x.Get() = fun s -> (s, s)
    member x.Put s = fun _ -> ((), s)

let state = new StateMonadBuilder()
 
let inc () =
    state {
        let! a = state.Get()
        return! state.Put(a + 1)
    }

let dec () =
    state {
        let! a = state.Get()
        return! state.Put(a - 1)
    }

let addBy v =
    state {
        let! a = state.Get()
        return! state.Put(v + a)
    }

let run () =
    state {
        do! addBy 10
        let! x = state.Get()
        do! inc()
        let! x = state.Get()
        do! dec()
        let! x = state.Get()
        printfn "last %A" x
        return x
    }

10 |> run ()
