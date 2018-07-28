///
/// DECORATOR
/// 
/// Attach additional responsibilities to an object dynamically. Decorators provide a flexible
/// alternative to subclassing for extending functionality.
/// 

///
/// REMARKS
/// 
/// If pre-function decoration is used then the signature of the decorator's input and output has
/// to be the same as the input of the core function. Also there can be only single input parameter.
/// However, tuple may be used to give actually more parameters. That is:
/// 
///   type coreFunction : (a * b) -> c    =>    type preDecorator : (a * b) - > (a * b)
/// 
/// 
/// If post-function decoration is used then the signature of the decorator's input and output has
/// to be the same as the output of the core function. That is:
/// 
///   type coreFunction : (a -> b) -> c    =>    type postDecorator : c -> c   
/// 
/// Seems that pre-decoration is mapping parameters (input) and post-decoration is mapping the
/// result (output). In both cases it is transparent for the caller if the function is decorated or
/// not.
/// 
/// In pure functional context the post function decorator is more powerful. This is due that in
/// pure functional world everything a function does is the result. If the core function has some
/// fixed values to calculate the result, these cannot be altered by the pre-decoration. Clearly
/// this is not the case with post-decoration, which can alter the result.
/// 
/// 
/// There is also third kind of decorator, which can operate as pre- and post-decorator. It does
/// both maps the input parameters and also the output. However, these kind of decorators cannot
/// be chained together without additional effort. Signature of this kind of decorator is:
/// 
///   type coreFunction (a -> b) -> c    =>    type fullDecorator : (a - b) -> c
/// 


///
/// Example
/// 
/// This example demonstrates two kind of decorations. First one is pre function decorator, which
/// turns every rectangle to a square. Second one is post function decorator, which smoothes the 
/// cornes of every rectangle to have same roundness.
/// 
/// 
/// NOTES
/// 
/// Pre-decorator cannot make rounded rectangles, since the core function does not support this via
/// parameters.
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
