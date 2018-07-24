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

///
/// Example
/// 


type BitmapRectangle = { Pixels : (int * int) list }
type VectorRectangle = { Width : int; Height : int }

type BitmapCircle = { Pixels : (int * int) list }
type VectorCircle = { Radius : int }

type Rectangle = BitmapRectangle of BitmapRectangle | VectorRectangle of VectorRectangle
type Circle = BitmapCircle of BitmapCircle | VectorCircle of VectorCircle


type Factory = { CreateRectangle : int -> int -> Rectangle; CreateCircle : int -> Circle }


let vectorFactory = {
    CreateRectangle = (fun w h -> BitmapRectangle { Width = w; Height = h } )
    CreateCircle = (fun r -> Circle { VectorCircle.Radius = r })
}

let foo = VectorCircle({ Radius = 2 })
let bar = { Radius = 2 }

let f = VectorCircle(bar)





