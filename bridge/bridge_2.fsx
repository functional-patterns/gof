///
/// BRIDGE
/// 
/// Decouple an abstraction from its implementation so that the two can vary independently.
/// 

/// 
/// Structural, v2
/// 

///
/// APPLICABILITY
/// 
/// Use the Bridge pattern when
/// - you want to avoid a permanent binding between an abstraction and its implementation. This
///   might be the case, for example, when the implementation must be selected or switched at
///   run-time.
/// - both the abstractions and their implementations should be extensible by subclassing. In this
///   case, the Bridge pattern lets you combine the different abstractions and implementations and
///   extend them independently.
/// - changes in the implementation of an abstraction should have no impact on clients; that is,
///   their code should not have to be recompiled.
/// - (C++) you want to hide the implementation of an abstraction completely from clients. In C++
///   the representation of a class is visible in the class interface.
/// - you have a proliferation of classes as shown earlier in the first Motivation diagram. Such a
///   class hierarchy indicates the need for splitting an object into two parts. Rumbaugh uses the
///   term "nested generalizations" [RBP+91] to refer to such class hierarchies.
/// - you want to share an implementation among multiple objects (perhaps using reference
///   counting), and this fact should be hidden from the client. A simple example is Coplien's
///   String class [Cop92], in which multiple objects can share the same string representation
///   (StringRep).


///
/// NOTES
/// 
/// Find out if there is any sense to abstract actual data in functional programming. Usually data
/// has to be specific.
/// 


///
/// Example - Drawing application
/// 
/// In this example simple drawing program is demonstrated. Client can instantiate and use
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
    let paintPixeEllipse (origo : int * int) (radiusA : int) (radiusB : int) =
        printfn "painting pixel ellipse origo=%A radiusA=%i radiusB=%i" origo radiusA radiusB
























module Tester =
    let test() =
        // Partially apply abstraction side functions
        //let linePrimitive = PixelPainter.paintPixelLine
        //let ellipsePrimitive = PixelPainter.paintPixeEllipse
        let linePrimitive = VectorPainter.paintVectorLine
        let ellipsePrimitive = VectorPainter.paintVectorEllipse

        let drawShape = PainterAbstraction.drawShapeTemplate ellipsePrimitive linePrimitive

        let drawSnowman = WinterPainter.drawSnowmanTemplate drawShape
        let drawRobot = FuturePainter.drawRobotTemplate drawShape

        drawSnowman
        drawRobot



        // PixelPainter.paintPixelLine (0, 0) (1, 10)
        // VectorPainter.paintVectorLine (0, 0) (10, 20)
        // VectorPainter.paintVectorEllipse (10, 20) 5 12

Tester.test()