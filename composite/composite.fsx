///
/// COMPOSITE
///
/// Compose objects into tree structures to represent part-whole hierarchies. Composite lets clients
/// treat individual objects and compositions of objects uniformly.
///

///
/// Example
/// 
/// In this simple example a discriminated union is used to present a shape. A shap may be a single
/// circle, single square or composite (aka any combination of those).
/// 
/// Client code may use any of the functions supporting the shape type without knowing if the shape
/// is a single item or a more complex composition.
/// 

// Simple discriminated union type to present complex shapes
type Shape = Circle of int | Square of int | Composite of Shape list

// A function to calculate size of the shape
let rec area shape =
    match shape with
        | Circle radius ->
            let pi = 3
            radius * radius * pi
        | Square size ->
            size * size
        | Composite parts ->
            parts |> List.fold (fun state shape -> state + area shape) 0


let test() =
    let square : Shape = Square 2
    let circle : Shape = Circle 10
    let composite : Shape = Composite [ Square 2; Square 4 ]

    printfn "size of %A is %A" square (area square)
    printfn "size of %A is %A" circle (area circle)
    printfn "size of %A is %A" composite (area composite)

test()
