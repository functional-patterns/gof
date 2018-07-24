


// type State = int
// type Computation<'v> = 'v * State

// let get s = (snd s, snd s)
// let put v _ = ((), v)



// get (2, 10)
// put 2 ('c', 10)


type State<'s, 'a> = 's -> 'a * 's

/// 'a -> State s a
/// 'a -> ('s -> ('a * 's))
let ret (x : 'a) : State<'s, 'a>  = fun s -> (x, s)


(ret 5) 1

// s -> (s -> ('s * 's))
let get = fun s -> (s, s)

get 2

// 
let put x = fun _ -> ((), x)

(put 5) 1


