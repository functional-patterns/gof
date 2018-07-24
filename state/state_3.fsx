// Haskell is a pure language and because of that, our programs are made of functions that can't
// change any global state or variables, they can only do some computations and return them
// results. This restriction actually makes it easier to think about our programs, as it frees us
// from worrying what every variable's value is at some point in time. However, some problems are
// inherently stateful in that they rely on some state that changes over time. While such problems
// aren't a problem for Haskell, they can be a bit tedious to model sometimes. That's why Haskell
// features a thing called the state monad, which makes dealing with stateful problems a breeze
// while still keeping everything nice and pure.

type Stack = int list
type State = Stack -> (int * Stack)

let pop (stack : Stack) : (int * Stack) =
    match stack with
        | h::t -> (h, t)

let push (a : int) (s : Stack) : (unit * Stack) = ((), a :: s)



let pure (x : int) : State = fun s -> (x, s)


let (>>=) (h : State) (f : int -> State) =
    fun s ->
        let (a, newState) = h s
        let g = f a
        g newState

let get = fun s -> (s, s)
let put newState = fun _ -> ((), newState)

pure [1; 2; 3] >>= pop






    



