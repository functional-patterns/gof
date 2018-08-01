
module Simple =
    type Number = N of float

    type Number with
        static member Add ((N a), (N b)) : Number =
            N (a + b)
        static member Sub (N a) : Number =
            N (a - 1.0)
        static member Mul ((N a), (N b)) : Number =
            N (a * b)

module Complex =
    type Number = N of float * float

    type Number with
        static member Add ((N (a1, a2)), (N (b1, b2))) : Number =
            N (a1 + b1, a2 + b2)
        static member Sub ((N (a1, a2)), (N (b1, b2))) : Number =
            N (a1 - b1, a2 - b2)
        static member Mul ((N (a1, a2)), (N (b1, b2))) : Number =
            N (a1 * b1, a2 * b2)

let inline twice (a : ^t) : ^t =
    let add = (^t : (static member Add : ^t * ^t -> ^t) (a, a))
    //let sub = (^t : (static member Sub : ^t -> ^t) a)
    //(^a : (static member check : ^a list -> bool) system)
    add

let inline fubby (a : ^t) (b : ^t) : ^t =
    (^t : (static member Mul : ^t -> ^t -> ^t) (a, b))

let s = Simple.N 5.0

twice s

fubby s s

