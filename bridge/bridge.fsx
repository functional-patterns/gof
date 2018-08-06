///
/// BRIDGE
/// 
/// Decouple an abstraction from its implementation so that the two can vary independently.
/// 

///
/// NOTES
/// 
/// Find out if there is any sense to abstract actual data in functional programming. Usually data
/// has to be specific.
/// 

///
/// CONCLUSION
/// 
/// Bridge pattern has a different form in functional than in object-oriented programming. However,
/// the same idea may be used to totally separate the abstraction from the implementation.
/// 

///
/// Example - Drawing application
/// 
/// In this example simple drawing application is implemented. Client can instantiate and use
/// different paletettes to draw shapes. Actual drawing is done on the implementation side, which
/// offers vector and pixel based implementations.
/// 
/// Client side of the bridge can vary independently from the implementation. New palettes can be
/// created without touching to implementation side. 
/// 
/// Since client side (palette) uses implementation side (painter) only via interface functions,
/// both can vary independently. Only constraint is that implementation side offers the same set of
/// primitive painting functions.
/// 

///
/// Functions of the abstraction part (Window, TransientWindow, IconWindow)
/// 
module PainterAbstraction =
    ///
    /// Abstract data, concerete implementation may use something different
    /// 
    type Point = int * int
    type Shape = Circle of int * Point | Rectangle of Point * Point

    let drawShapeTemplate ellipsePrimitive linePrimitive shape =
        match shape with
            | Circle (radius, (origoX, origoY)) ->
                ellipsePrimitive (origoX, origoY) radius radius
            | Rectangle ((x1, y1), (x2, y2)) ->
                linePrimitive (x1, y1) (x2, y1)
                linePrimitive (x1, y2) (x2, y2)
                linePrimitive (x1, y1) (x1, y2)
                linePrimitive (x2, y1) (x2, y2)
        


module FuturePainter =
    let drawRobotTemplate drawShape =
        printfn "drawing robot"
        
        let body = PainterAbstraction.Rectangle ((-5, 10), (5, 0))
        let head =  PainterAbstraction.Rectangle ((-4, 14), (4, 10))
        let leftEye =  PainterAbstraction.Circle (1, (-3, 13))
        let rightEye =  PainterAbstraction.Circle (1, (3, 13))

        drawShape body
        drawShape head
        drawShape leftEye
        drawShape rightEye


module WinterPainter = 
    let drawSnowmanTemplate drawShape =
        printfn "drawing snowman"

        let bottomBall =  PainterAbstraction.Circle (10, (0, 0))
        let middleBall =  PainterAbstraction.Circle (8, (0, 9))
        let topBall =  PainterAbstraction.Circle (6, (0, 16))

        drawShape bottomBall
        drawShape middleBall
        drawShape topBall




///
/// Functions of the implementation part (WindowImp, XWindowImp, PmWindowImp)
/// 
/// Implementation has abstraction to draw only straight lines and arcs. One implementation
/// is using vector graphics and one implementation is using pixel graphics.
/// 
/// Template postfix indicates abstract base function, which should be partially applicated
/// to create a concerete painter.
/// 
module PainterImplementation =
    let paintLineTemplate linePainter p1 p2 =
        linePainter p1 p2

    let paintEllipseTemplate ellipsePainter origo radiusA radiusB =
        ellipsePainter origo radiusA radiusB


module VectorPainter =
    let paintVectorLine (p1 : int * int) (p2 : int * int) =
        printfn "painting vector line from %A to %A" p1 p2
    let paintVectorEllipse (origo : int * int) (radiusA : int) (radiusB : int) =
        printfn "painting vector ellipse origo=%A radiusA=%i radiusB=%i" origo radiusA radiusB



module PixelPainter =
    let paintPixelLine (p1 : int * int) (p2 : int * int) =
        printfn "painting pixel line from %A to %A" p1 p2
    let paintPixelEllipse (origo : int * int) (radiusA : int) (radiusB : int) =
        printfn "painting pixel ellipse origo=%A radiusA=%i radiusB=%i" origo radiusA radiusB


let test() =
    // Partially apply abstraction side functions
    let linePrimitive = PixelPainter.paintPixelLine
    let ellipsePrimitive = PixelPainter.paintPixelEllipse
    //let linePrimitive = VectorPainter.paintVectorLine
    //let ellipsePrimitive = VectorPainter.paintVectorEllipse

    let drawShape = PainterAbstraction.drawShapeTemplate ellipsePrimitive linePrimitive

    let drawSnowman = WinterPainter.drawSnowmanTemplate drawShape
    let drawRobot = FuturePainter.drawRobotTemplate drawShape

    drawSnowman
    drawRobot

test()
