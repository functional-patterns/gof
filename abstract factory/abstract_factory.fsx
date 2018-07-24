///
/// ABSTRACT FACTORY
/// 
/// Provide an interface for creating families of related or dependent objects without specifying
/// their concrete classes.
/// 

///
/// NOTES
/// 
/// It is as important to create data in both - functional and object-oriented software - so the
/// design patterns seems useful at the fist glance.
/// 
/// Functions which do something with created data records have to support all the types. This can
/// be done with discriminated union. Compared to the object-oriented version there is clear
/// difference. Object-oriented version keeps the handling of a certain product inside a single
/// class whereas functional version keeps the handling of a certain operation inside a single
/// function.
/// 

///
/// QUESTION
///  
/// Could the functions to operate with the items also be passed to the client? In this case it
/// could be possible to eliminate the need of the discriminated union. Instead a function could
/// use the concrete type.
/// 
/// How a 3rd party plugin/implementation can be added? If discriminated unions are used, then
/// there is no way to make 3rd party plugins.
/// 

///
/// Example
/// 

module Bitmap =
    type Rectangle = { Pixels : (int * int) list }
    type Circle = { Pixels : (int * int) list }
    let draw circle = printfn "bitmap circle with pixels %A" circle.Pixels

module Vector = 
    type Rectangle = { Width : int; Height : int }
    type Circle = { Radius : int }
    let draw circle = printfn "vector circle with radius %A" circle.Radius

type Rectangle = BitmapRectangle of Bitmap.Rectangle | VectorRectangle of Vector.Rectangle
type Circle = BitmapCircle of Bitmap.Circle | VectorCircle of Vector.Circle


type Factory = { CreateRectangle : int -> int -> Rectangle; CreateCircle : int -> Circle }


let bitmapFactory = {
    CreateRectangle = (fun w h -> BitmapRectangle({ Pixels = List.empty }))
    CreateCircle = (fun r -> BitmapCircle({ Pixels = List.empty }))
}

let vectorFactory = {
    CreateRectangle = (fun w h -> VectorRectangle({ Width = w; Height = h }))
    CreateCircle = (fun r -> VectorCircle({ Radius = r }))
}

let draw circle =
    match circle with
    | BitmapCircle c ->
        printfn "drawing bitmap circle"
    | VectorCircle c ->
        printfn "drawin vector circle with radius = %A" c.Radius


let client factory =
    let circle = factory.CreateCircle 2
    draw circle

client bitmapFactory


type FunctionSet = { DrawFunction : unit -> unit; AreaFunction : unit -> int }
type CircleFactory = { CreateCircle : int -> FunctionSet }
type RectangleFactory = { CreateRectangle : int * int -> FunctionSet }

let vectoryCircleFactory = { CreateCircle = (fun radius -> 
    {
        DrawFunction = printfn "drawin vector circle with radius = %A" radius;
        AreaFunction = radius * radius * 3
    }
)}


let client2 factory functionSet =

// Homma sotii functionaalista paradigmaa vasten?


//let add (a : int) (b : int) = a + b
//let add (a : float) (b : float) = a + b