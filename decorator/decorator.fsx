///
/// DECORATOR
/// 
/// Attach additional responsibilities to an object dynamically. Decorators provide a flexible
/// alternative to subclassing for extending functionality.
/// 

///
/// Example
/// 
/// This example demonstrates two kind of decorations. First one is pre function decorator, which
/// turns every rectangle to a square. Second one is post function decorator, which smoothes the 
/// cornes of every rectangle to have same roundness.
/// 

///
/// Simple type of rectangle which may or may not have rounded corners
/// 
type Rectangle = { Width : int; Height : int; Roundness : int }

///
/// Function to create simple rectangles with straight corners
/// 
let createRectangle width height =
    { Width = width; Height = height; Roundness = 0 }

///
/// Decorator to make rectangles with same height and width (aka squares) and rounded corners
/// 
let fullDecorator coreFunction width _ =
    let result = coreFunction width width
    { result with Roundness = 5 }

///
/// Decorator to make rectangles with same height and width (aka squares)
/// 
let squarePreDecorator (width, _) =
    (width, width)

///
/// Decorator to make cornes round
/// 
let roundPostDecorator rectangle =
    { rectangle with Roundness = 5 }


let test() =
    let rectangleCreator width height = createRectangle width height |> roundPostDecorator

    let roundedSquareCreator width height = fullDecorator createRectangle width height 

    let squareCreator width height = (width, height)
                                        |> squarePreDecorator
                                        |> (fun t -> createRectangle (fst t) (snd t))

    printfn "rectangle: %A" (rectangleCreator 5 10)
    printfn "rounded square: %A" (roundedSquareCreator 5 10)
    printfn "square: %A" (squareCreator 5 10)

test()
