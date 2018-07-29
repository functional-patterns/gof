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
/// to use Memoize pattern with lazy evaluation. Laze evaluation enables dynamic creation of of the
/// flyweight records from the delivered key. If the data cannot be generated from the key, then it
/// id merely just an id and the map has to be populated statically.
///
/// Usual way to implement Memoize pattern in F# is to use mutable data. However, this can be done
/// also with immutable data and lazy evaluation. In Haskell the map type supports lazy evaluation
/// by default. For more information see https://wiki.haskell.org/Memoization.
///

///
/// Example
/// 
/// In this example a scenery is created from rocks and trees. Most of the rocks and trees are the
/// same, so they can be presented by the same data item. Multiple trees can be combined to create
/// a forest.
/// 
/// Example demonstrates how using the flyweight pattern in F# reduces the memory usage. The benefit
/// is greater on bigger flyweights.
/// 

// General definitions
type Shape = Circle | Square | Triangle
type Color = Green | Blue | Gray

// External data of flyweights (not shared)
type Placement = { X : int; Y : int }

// Inner data of different flyweights (shared among)
type Rock = { Shape : Shape; Color : Color; Size : int }
type Tree = { Shape : Shape; Color : Color; }
type Forest = (Tree * Placement) list

// SceneryItem can be a tree, a rock or a forest (corresbonds to base class for the structure)
type SceneryItem = Tree of Tree | Rock of Rock | Forest of Forest

// Pre-determined tree and rock types
type TreeType = Oak | Fir | Mapple
type RockType = Small | Big

let treeFactory treeType =
    let oak = { Shape = Circle; Color = Green }
    let fir = { Shape = Triangle; Color = Green }
    let mapple = { Shape = Square; Color = Green }

    match treeType with
    | Oak -> oak
    | Fir -> fir
    | Mapple -> mapple

let rockFactory rockType =
    let small = { Shape = Circle; Color = Gray; Size = 3 }
    let big = { Shape = Square; Color = Gray; Size = 10 }

    match rockType with
    | Small -> small
    | Big -> big


let flyweightClient() = 
    System.GC.Collect()
    let memoryBefore = System.GC.GetTotalMemory(true)

    let forest1 = List.init 10000000 (fun _ -> treeFactory Fir)
    let forest2 = List.init 10000000 (fun i -> if i % 2 = 0 then Oak else Mapple)

    System.GC.Collect()
    let memoryAfter = System.GC.GetTotalMemory(true)

    memoryAfter - memoryBefore


let regularClient() = 
    System.GC.Collect()
    let memoryBefore = System.GC.GetTotalMemory(true)

    let forest1 = List.init 10000000 (fun _ -> { Shape = Triangle; Color = Green })
    let forest2 = List.init 10000000 (fun i ->
        if i % 2 = 0 then { Shape = Circle; Color = Green } else { Shape = Square; Color = Green })

    System.GC.Collect()
    let memoryAfter = System.GC.GetTotalMemory(true)

    memoryAfter - memoryBefore


let test() =
    let flyweightMemoryUsage = flyweightClient() / (1024L * 1024L)
    let regularMemoryUsage = regularClient() / (1024L * 1024L)

    printfn "flyweight memory usage: %A MB" flyweightMemoryUsage
    printfn "regular memory usage: %A MB" regularMemoryUsage

test()
