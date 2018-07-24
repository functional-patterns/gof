// Haskell is a pure language and because of that, our programs are made of functions that can't
// change any global state or variables, they can only do some computations and return them
// results. This restriction actually makes it easier to think about our programs, as it frees us
// from worrying what every variable's value is at some point in time. However, some problems are
// inherently stateful in that they rely on some state that changes over time. While such problems
// aren't a problem for Haskell, they can be a bit tedious to model sometimes. That's why Haskell
// features a thing called the state monad, which makes dealing with stateful problems a breeze
// while still keeping everything nice and pure.


let push (a : 'a) (s : 'a list) : (unit * 'a list) = ((), a :: s)
let pop (s : 'a list) : ('a * 'a list) =
    match s with
        | h::t -> (h, t)


let pure x = fun s -> (x, s)

type State<'a, 's> = ('a * 's)

let z : State<int, int list> = (0, [1;2])

let (>>=) (h : 's -> ('a * 's)) (f : 'a -> ('s -> 'a * 's)) =
    fun s ->
        let (a, newState) = h s
        let g = f a
        g newState

// Type of the State monad is (state -> (x, state))
    



