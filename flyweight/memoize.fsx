module LazyList =
    type Data<'T> = Empty | Node of Lazy<'T> * Lazy<Data<'T>>

    let item index ls =
        let rec indexer n ls =
            match ls with
                | Node (a, ls) ->
                    if n = 0
                    then a.Force()
                    else indexer (n - 1) (ls.Force())
        indexer index ls

    let rec initInfinite (index : int) (f : int -> 'a) : Data<'a> =
        let a = lazy (f index)
        let ls = lazy (initInfinite (index + 1) f)
        Node (a, ls)
    
module Memoize =
    let memoize (f : int -> 'a) : (int -> 'a) =
        let inifinityList = (LazyList.initInfinite 0 f)
        fun i -> (inifinityList |> LazyList.item i)

let treeFactory key =
    let trees = List.init 10 (fun i -> List.replicate 10000 i)
    trees |> List.item key
    // let treeOne = List.replicate 1000 0
    // let treeTwo = List.replicate 1000 1

    // if key = 0 then treeOne else treeTwo
    //List.replicate 10000 key

let clientTemplate memoize =
    System.GC.Collect()
    let memoryBefore = System.GC.GetTotalMemory(true)

    let memo = memoize treeFactory
    let forest = List.init 1000 (fun i -> memo (i % 10))

    System.GC.Collect()
    let memoryAfter = System.GC.GetTotalMemory(true)

    printfn "forest size=%A" forest.Length

    memoryAfter - memoryBefore

let flyweightClient() = clientTemplate Memoize.memoize
let regularClient() = clientTemplate id


regularClient() / 1024L / 1024L
flyweightClient() / 1024L / 1024L


let rec prng count modulus a c seed =
    match count with
        | 0 ->
            []
        | _ ->
            let seed = (a * seed + c) % modulus
            seed::(prng (count - 1) modulus a c seed)

prng 10 (1 <<< 8) 1664525 1013904223 5


//let ls = LazyList.Node (5, LazyList.LazyList<int>.Empty)
let ls = LazyList.Data<int>.Node (lazy(5), lazy(LazyList.Data<int>.Empty))