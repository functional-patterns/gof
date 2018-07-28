///
/// VISITOR
/// 
/// Represent an operation to be performed on the elements of an object structure. Visitor lets you
/// define a new operation without changing the classes of the elements on which it operates.
/// 

///
/// NOTE
/// 
/// For lists and maps functional programms usually define a fold function. It is the same thing as
/// the listTraverser function in the following example.
/// 
/// It would also be possible to define traverser function for a tree. In this case the very same
/// visitors could be used.
/// 

///
/// Example
/// 
/// In this example data structure contains list of shapes. First a recursive traverser function is
/// defined to go through the data structure. Then two different visitors are defined to visit each
/// element. 
/// 

///
/// Shapes may be circles with radius or rectangles with height and width
/// 
type Shape = Circle of int | Rectangle of int * int

let rec listTraverser visitor acc shapes =
    match shapes with
        | [] ->
            acc
        | head::tail ->
            let acc = visitor acc head
            listTraverser visitor acc tail
            
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

[ Circle 5; Rectangle (2, 5); Circle 4] |> listTraverser printVisitor ()
[ Circle 5; Rectangle (2, 5); Circle 4] |> listTraverser circleCollector []


let shapes = [ Circle 5; Rectangle (2, 5); Circle 4]

shapes |> List.fold printVisitor ()


let test() =
    let shapes = [ Circle 5; Rectangle (2, 5); Circle 4]

    shapes |> listTraverser printVisitor ()
    shapes |> listTraverser circleCollector []
