


// type State = int
// type Computation<'v> = 'v * State

// let get s = (snd s, snd s)
// let put v _ = ((), v)



// get (2, 10)
// put 2 ('c', 10)


type A = float
type S = int
type State<'a> = S -> 'a * S

/// A -> State s a
/// A -> (S -> (A * S))
let ret a : State<'a> = fun s -> (a, s)


(ret 5.0) 1

// s -> (s -> (S * S))
let get = fun s -> (s, s)

get 2

// 
let put x = fun _ -> ((), x)

(put 5) 1


//(>>=) :: State s a -> (a -> State s b) -> State s b



let runState = id
///
/// (s -> 'a * s) -> ('a -> s -> 'b * s) -> ('s -> 'b * s)
/// 
/// 5
/// a  = fst (a 5) )
/// s0 = fst (a 5)
/// 
/// b = fst f a s0
/// 
/// 
/// 
let bind (a : State<'a>) (f : 'a -> State<'b>) : State<'b> =
    let a' = runState p
    let k' = runState >> f

    let s0 = 

    // let p' = runState p     // p' :: s -> (a, s)
    // let k' = runstate >> k  // k' :: a -> s -> (b, s)
    // let (x, s1) = p' s0     // (x, s1) :: (a, s)
    // let (y, s2) = k' x s1   // (y, s2) :: (b, s)



// (>>=) :: State s a -> (a -> State s b) -> State s b
// p >>= k = q where
//     p' = runState p        -- p' :: s -> (a, s)
//     k' = runState . k      -- k' :: a -> s -> (b, s)
//     q' s0 = (y, s2) where  -- q' :: s -> (b, s)
//         (x, s1) = p' s0    -- (x, s1) :: (a, s)
//         (y, s2) = k' x s1  -- (y, s2) :: (b, s)
//     q = State q'