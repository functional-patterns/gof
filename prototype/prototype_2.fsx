///
/// PROTOTYPE
/// 
/// Specify the kinds of objects to create using a prototypical instance, and create new objects by
/// copying this prototype.
/// 

/// Plugin tyyppinen arkkitehtuuri funktionaalisesti. Käytiin läpi edellisessä palaverissa.


type Radius = int
type Height = int
type Width = int
type Position = int * int

type Shape = Circle of Position * Radius | Rectangle of Position * Width * Height

let insertionToolTemplate (prototype : Shape) positioner (position : Position) : Shape =
    positioner prototype position


let shapePositioner shape position =
    match shape with
        | Circle (_, radius) ->
            Circle (position, radius)
        | Rectangle (_, width, height) ->
            Rectangle (position, width, height)


let circleInsertionTool = insertionToolTemplate (Circle ((0, 0), 5)) shapePositioner
let rectangleInsertionTool = insertionToolTemplate (Rectangle ((0, 0), 5, 3)) shapePositioner


circleInsertionTool (10, 3)
rectangleInsertionTool (10, 3)




let insertionTool = rectangleInsertionTool

insertionTool (10, 3)