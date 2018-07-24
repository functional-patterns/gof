//
// COMPOSITE
//
// Compose objects into tree structures to represent part-whole hierarchies. Composite lets clients
// treat individual objects and compositions of objects uniformly.
//

//
// NOTES
//
// Type structure of Composite is implemented with discriminated union in functional programming.
// Functionality is set of functions operating with the discriminated union type. Traversing
// through the composite is done by recursion.
//
// Recursion function can be parameterized to take arbitrary functions to perform different 
// operations to the items 
//
// More information from Real-World Functional Programming page 200
//
//

///
/// REMOVING COMPONENT
/// 
/// Removing of a component requires an identification to be added to all components. In object-
/// oriented world all objects have a unique id since pointer (memory address) usage. This is not
/// supported in functional world.
/// 
/// It also leads to some confusing situations to allow removel of components. For example
/// removal the root element from the graph results to what? 
/// 


//=================================================================================================
// Example - Begin
//=================================================================================================

module Example1 =
    // Define the type hierarchy
    type Graphic = Picture of Picture | Line of Line | Rectangle of Rectangle | Text of Text
    and Picture = { Parts : Graphic list }
    and Line = { From : (int * int); To : (int * int) }
    and Rectangle = { TopLeft : (int * int); BottomRight : (int * int) }
    and Text = { String : string; Position : (int * int) }


    // Define the functionality of the types

    ///
    /// Adds a new part to an existing graphic
    /// 
    let add part graphic =
        match graphic with
            | Picture(picture) ->
                printfn "appending to picture"
                Graphic.Picture { Parts = List.append picture.Parts [part] }
            | Line(line) ->
                printfn "appending to line"
                Graphic.Picture { Parts = [ Graphic.Line line; part ] }
            | Rectangle(rectangle) ->
                printfn "appending to rectangle"
                Graphic.Picture { Parts = [ Graphic.Rectangle rectangle; part ] }
            | Text(text) ->
                printfn "appending to text"
                Graphic.Picture { Parts = [ Graphic.Text text; part ] }

    ///
    /// 
    /// 
    // let rec remove graphic part =
    //     match graphic with
    //         | Picture(picture) ->
    //             picture.Parts |> 



    ///
    /// Counts how many components the graphic contains
    /// 
    let rec componentCount (graphic : Graphic) : int =
        match graphic with
            | Picture(picture)
                -> 1 + (picture.Parts |> Seq.sumBy componentCount)
            | _ -> 1

    let line : Graphic = Line { From = (10, 10); To = (20, 10) }
    let rect : Graphic = Rectangle { TopLeft = (10, 10); BottomRight = (20, 10) }

    let pict : Graphic = Picture { Parts = [ line; rect] }



    line
    |> add line
    |> add rect
    |> add pict

//=================================================================================================
// Example - End
//=================================================================================================

module Example2 =

    type Composite = Leaf of Leaf | Node of Node
    and Leaf = string
    and Node = Composite list

    let draw composite =
        match composite with
            | Composite.Leaf(leaf) -> printf " %s " leaf
            | Composite.Node(_) -> ()


    let rec forAll func composite =
        match composite with
            | Leaf(_) ->
                func composite
            | Node(node) -> 
                node |> List.iter (fun t ->
                    forAll func t)

    let add part composite =
        match composite with 
            | Node(node) ->
                Composite.Node (List.append node [part])
            | Leaf(_) ->
                Composite.Node (List.append [composite] [part])


open Example2

let brown = Composite.Leaf "a brown"
let fox = Composite.Leaf "fox"
let jumped = Composite.Leaf "jumped"
let over = Composite.Leaf "over"
let sleepy = Composite.Leaf "a sleepy"
let dog = Composite.Leaf "dog"

let first = add fox brown
let second = add over jumped
let third =  add dog sleepy

let root =
    Composite.Node []
    |> add first
    |> add second
    |> add third


forAll draw root

forAll draw brown

let draw = forAll Example2.draw

draw brown
draw root