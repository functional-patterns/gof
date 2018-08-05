///
/// FLYWEIGHT
///
/// Use sharing to support large numbers of fine-grained objects efficiently.
///

///
/// CONCLUSION
///
/// In F# flyweight design pattern (sharing a record) is benefical in terms of memory usage. Sharing
/// improves the memory efficiency in proportion of the shared record size and count. For small
/// records the memory usage of the container nullifies the benefit.
///
/// In simple case the flyweight factory can be just prepopulated map of values. However, using
/// pre-populated map does not allow dynamic creation of the flyweights. Alternative for this is
/// to use Memoize pattern with lazy evaluation. Lazy evaluation enables dynamic creation of of the
/// flyweight records from the delivered key. If the data cannot be generated from the key, then it
/// is merely just an id and the map has to be populated statically.
///
/// Usual way to implement Memoize pattern in F# is to use mutable data. However, this can be done
/// also with immutable data and lazy evaluation. In Haskell the map type supports lazy evaluation
/// by default. For more information see https://wiki.haskell.org/Memoization.
///

///
/// EXAMPLE
/// 
/// This example demonstrates the impact to memory usage and speed when flyweight is used. Program
/// generates a forest of 100 trees. A forest is modeled with three different tree types - oaks,
/// maples and beeches - which are pesudo-randomly positioned. The constant data utilized by the 
/// flyweight is the tree, which always have the same 10 000 specific leaves. The variating data is
/// the pseudo-random position of an individual tree.
/// 

module LazyList =
    type List<'a> = Empty | Node of Lazy<'a> * Lazy<List<'a>>
    let item index ls =
        let rec indexer n ls =
            match ls with
                | Node (a, ls) ->
                    if n = 0
                    then a.Force()
                    else indexer (n - 1) (ls.Force())
        indexer index ls

    let rec initInfinite (f : int -> 'a) : List<'a> =
        let rec initializer index f =
            let a = lazy (f index)
            let ls = lazy (initializer (index + 1) f)
            Node (a, ls)
        initializer 0 f


module Flyweight =
    let memoize (f : int -> 'a) : (int -> 'a) =
        let inifinityList = (LazyList.initInfinite f)
        fun i -> (inifinityList |> LazyList.item i)
    

module Framework =
    // Some leaf types to create different trees
    type Leaf = Oak | Maple | Beech
    
    // Factory to create various trees
    let treeFactory key =
        let oak = List.replicate 10000 Leaf.Oak
        let maple = List.replicate 10000 Leaf.Maple
        let beech = List.replicate 10000 Leaf.Beech

        [oak; maple; beech] |> List.item (key % 3)

    // Pseudo-random number generator (not essential for the example)
    let rec prng count modulus a c seed =
        match count with
            | 0 ->
                []
            | _ ->
                let seed = (a * seed + c) % modulus
                seed::(prng (count - 1) modulus a c seed)
        
    // Client creating a forest
    let clientTemplate memoize =
        let treeCount = 100

        System.GC.Collect()
        let memoryBefore = System.GC.GetTotalMemory(true)

        // Flyweights are used or not depending of the memoize implementation
        let memo = memoize treeFactory

        // Constant data is the actual tree (Oak, Maple or Beech)
        let trees = List.init treeCount (fun i -> memo (i % 10))

        // Variating data is the position of the tree
        let xPositions = prng treeCount (1 <<< 8) 1664525 1013904223 11
        let yPositionss = prng treeCount (1 <<< 8) 1664525 1013904223 17

        let forest = List.zip3 trees xPositions yPositionss

        System.GC.Collect()
        let memoryAfter = System.GC.GetTotalMemory(true)

        printfn "forest size=%A" forest.Length

        ((memoryAfter - memoryBefore), forest)

let test() =
    // Flyweight client uses memoize pattern to reduce runtime and memory usage
    let flyweightClient() = Framework.clientTemplate Flyweight.memoize

    // Regular client uses treeFactory directly without caching
    let regularClient() = Framework.clientTemplate id

    let toMegaBytes bytes = bytes / 1024L / 1024L

    let (regularMemory, regularForest) = regularClient()
    let (flyweightMemory, flyweightForest) = regularClient()

    printfn "reglar memory concumption %A MB" (regularMemory |> toMegaBytes)
    printfn "flyweight memory concumption %A MB" (flyweightMemory |> toMegaBytes)
    printfn "identical forests = %A" (regularForest = flyweightForest)

test()
