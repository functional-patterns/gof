///
/// FACTORY METHOD
///
/// Define an interface for creating an object, but let subclasses decide which class to
/// instantiate. Factory Method lets a class defer instantiation to subclasses. 
/// 


module Vector =
    type Circle = Circle of int
    type Rectangle = Rectangle of int * int

module Bitmap =
    type Circle = Circle of int list
    type Rectangle = Rectangle of int list

type A = Vector.Circle
type B = B of Bitmap.Circle
type Circle = C | B



type RectangleCreator = int -> Circle


