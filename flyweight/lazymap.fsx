
// let memoize fn =
//   let cache = new System.Collections.Generic.Dictionary<_,_>()
  
//   fun key ->
//     match cache.TryGetValue key with
//     | true, value ->
//         value
//     | false, _ ->
//         let value = fn (key)
//         cache.Add(key, value)
//         value

let memoize (f : int -> float) : (int -> float) =
    let s = Seq.initInfinite (fun i -> float (10 * i))
    fun index -> Seq.item index s

let summa x = [0.0 .. float x] |> List.sum |> float

summa 0
summa 100
summa 10000000

let summaMemo = memoize summa

printfn "%A" (summaMemo 100000000)
printfn "%A" (summaMemo 100000000)


let a : Lazy<int> = lazy(summaMemo 500000000 |> int)
printfn "%A" (a.Force())
printfn "%A" (a.Force())

type LazyList' = Empty' | Node' of int * LazyList'

type LazyList' with
    static member Item index ls =
        let rec indexer n ls =
            match ls with
                | Node' (a, ls) ->
                    if n = 0
                    then a
                    else indexer (n - 1) ls
        indexer index ls


module Foo =
    type LazyList<'T> = Empty | Node of Lazy<'T> * Lazy<LazyList<'T>>

    type LazyList<'T> with
        static member Item index ls =
            let rec indexer n ls =
                match ls with
                    | Node (a, ls) ->
                        if n = 0
                        then a.Force()
                        else indexer (n - 1) (ls.Force())
            indexer index ls

    let lazyList = Node (lazy 10, lazy Node (lazy (List.sum [0.0 .. 50000000.0] |> int), lazy Node (lazy 30, lazy Empty)))

    lazyList |> LazyList<int>.Item 0

type LazyList<'T> = Empty | Node of Lazy<'T> * Lazy<LazyList<'T>>

type LazyList<'T> with
    static member Item index ls =
        let rec indexer n ls =
            match ls with
                | Node (a, ls) ->
                    if n = 0
                    then a.Force()
                    else indexer (n - 1) (ls.Force())
        indexer index ls



let rec initInfinite (index : int) (f : int -> 'a) : LazyList<'a> =
    let a = lazy (f index)
    let n = lazy (initInfinite (index + 1) f)
    Node (a, n)
    
let infinity = initInfinite 0 float

infinity |> LazyList<int>.Item 100000









let lazyList = Node (lazy 10, lazy Node (lazy (List.sum [0.0 .. 50000000.0] |> int), lazy Node (lazy 30, lazy Empty)))

lazyList |> LazyList<int>.Item 0

