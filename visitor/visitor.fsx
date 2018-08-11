///
/// VISITOR
/// 
/// Represent an operation to be performed on the elements of an object structure. Visitor lets you
/// define a new operation without changing the classes of the elements on which it operates.
/// 

///
/// Example
/// 
/// In this example data structure contains list of shapes. First a recursive traverser function is
/// defined to go through the data structure. Then two different visitors are defined to visit each
/// element. 
/// 
/// It would also be possible to define traverser function for a tree. In this case the very same
/// visitors could be used.
/// 

// Some shapes as discriminated union
type Shape = Circle of int | Rectangle of int * int

// Specify a traversal function (the default fold of the List could be used instead)
let rec listTraverser visitor acc shapes =
    match shapes with
        | [] ->
            acc
        | head::tail ->
            let acc = visitor acc head
            listTraverser visitor acc tail
            
// Specify some visitors
let printVisitor () shape =
    match shape with
        | Circle _ ->
            printfn "Circle"
        | Rectangle _ ->
            printfn "Rectangle"

let circleCollector acc shape =
    match shape with
        | Circle c ->
            Circle c::acc
        | _ ->
            acc

let test() =
    let shapes = [ Circle 5; Rectangle (2, 5); Circle 4]

    shapes |> listTraverser printVisitor ()
    shapes |> listTraverser circleCollector []

test()
