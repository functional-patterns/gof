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



