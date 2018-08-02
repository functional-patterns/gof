///
/// FACTORY METHOD
///
/// Define an interface for creating an object, but let subclasses decide which class to
/// instantiate. Factory Method lets a class defer instantiation to subclasses. 
/// 

///
/// CONCLUSION
/// 
/// Factory Method in functional programming is a simple function, which takes some input
/// parameters and returns the desired value. Framework code can then use these types, as long
/// as functions to operate with these values are also provided.
/// 
/// There is some options for the signature of the creator function. For example:
/// 
///   1) a -> b -> ^t, where ^t is a generic type
///   2) a -> b -> d,  where d is a discriminated union type
///   3) a -> b -> e,  where e is arbitrary type
/// 
/// In the first case statically resolved type parameters may be used to implement the generic
/// functions (F#) or typeclasses (Haskell).
/// 
/// In the second case basic functions are sufficient, as long as they operatate with all of the
/// discriminated union values.
/// 
/// In the third case set of functions to operate with the specific type 'e' has to be also
/// provided to the framework function, so it can operate with the created values
/// 

///
/// REMARKS
/// 
/// There is no point to create functions with Factory Method. Better way to achieve the same
/// result would be to use a function, which could then be partially applied by the framework
/// functions.
/// 

///
/// Example
/// 
/// In this example framework function requests a creator function (Factory Method) to be provided.
/// It then uses this function to create an initial shape. Size of the is then adjusted, until it
/// is slightly at least as big as requested. Requirement for the shapes returned by the creator
/// function is to provide Area and Increase functions.
/// 

module Round =
    type Circle = { Radius : int }

    type Circle with
        static member Increase circle =
            { Radius = circle.Radius + 1 }
        static member Area circle =
            let pi = 3
            circle.Radius * circle.Radius * pi

module Sharp =
    type Rectangle = { Width : int; Height : int}

    type Rectangle with
        static member Increase rectangle =
            { Width = rectangle.Width + 1; Height = rectangle.Height + 1 }
        static member Area rectangle =
            rectangle.Width * rectangle.Height


module Framework =
    let inline shapeAdjuster (creator : int -> ^t) (targetArea : int) : ^t =
        let shape = creator 5
        let rec adjuster shape =
            let currentArea = (^t : (static member Area : ^t -> int) shape)
            let increasedShape = (^t : (static member Increase : ^t -> ^t) shape)

            if currentArea < targetArea 
            then adjuster increasedShape
            else shape
            
        adjuster shape


let test() =
    let circleFactoryMethod = (fun s -> { Round.Circle.Radius = s})
    let squareFactoryMethod = (fun s -> { Sharp.Rectangle.Width = s; Sharp.Rectangle.Height = s})

    let circle = Framework.shapeAdjuster circleFactoryMethod 200
    let square = Framework.shapeAdjuster squareFactoryMethod 200

    printfn "circle of area %A has radius of %A" (Round.Circle.Area circle) circle.Radius
    printfn "square of area %A has size of %A" (Sharp.Rectangle.Area square) square.Width

test()
