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
/// Example 1
/// 

type Window = IconWindow | TransientWindow

type WindowImp = XWindowImp | PmWindowImp

let drawText = id
let winDrawRectTemplate devDrawText = id
let winDrawTextTemplate devDrawLine = id






let iconWindowDrawBorder  = id

let transientWindowDrawCloseBox = id


let windowImpDrawLetter = id
let windowImpDrawLine = id


let xWindowDrawLetter = id
let xWindowDrawLine = id



let pmWindowDrawLetter = id
let pmWindowDrawLine = id








///
/// Abstract data, real implementation may use something different
/// 
type Point = float * float
type Shape = Circle of float * Point | Rectangle of Point * Point


///
/// Functions of the abstraction part (Window, TransientWindow, IconWindow)
/// 
let drawWorldTemplate = id

let drawCircleWorld = id
let drawRectangleWorld = id


///
/// Functions of the implementation part (WindowImp, XWindowImp, PmWindowImp)
/// 
/// Implementation has abstraction to draw only straight lines and arcs. One implementation
/// is using vector graphics and one implementation is using pixel graphics.
/// 
/// Template postfix indicates abstract base function, which should be partially applicated
/// to create a concerete painter.
/// 
let paintLineTemplate = id
let paintArcTemplate = id


let paintVectorLine = id
let paintVectorArc = id


let paintPixelLine = id
let paintPixelArc = id
