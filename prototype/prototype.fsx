///
/// PROTOTYPE
/// 
/// Specify the kinds of objects to create using a prototypical instance, and create new objects by
/// copying this prototype.
/// 

///
/// EXAMPLE
/// 
/// Framework offers abstract insertion tool to add and position different types of items. Depending
/// of the type and values of the given prototype value, framework creates different items.
/// 
/// Two modules - Graphics and Music - are created, which are then used with the framework. Client
/// then partially applies the framework function(s) with specific items it wants to manipulate.
/// 

module Framework =
    type Position = int * int

    let insertionToolTemplate prototype positioner position =
        positioner prototype position

module Graphics =
    type Shape = Circle of Framework.Position * Radius | Rectangle of Framework.Position * Width * Height
    and Radius = int
    and Height = int
    and Width = int

    let shapePositioner shape position =
        match shape with
            | Circle (_, radius) ->
                Circle (position, radius)
            | Rectangle (_, width, height) ->
                Rectangle (position, width, height)

module Music =
    type Note = HalfNote of Framework.Position | FullNote of Framework.Position

    let notePositioner note position =
        match note with
            | HalfNote _ ->
                HalfNote position
            | FullNote _ ->
                FullNote position


let test() =
    let circleInsertionTool =
        Framework.insertionToolTemplate (Graphics.Circle ((0, 0), 5)) Graphics.shapePositioner
    let rectangleInsertionTool =
        Framework.insertionToolTemplate (Graphics.Rectangle ((0, 0), 5, 3)) Graphics.shapePositioner

    let art = [circleInsertionTool (5, 5); rectangleInsertionTool (10, 2)]
    printfn "art: %A" art


    let halfNoteInsertionTool =
        Framework.insertionToolTemplate (Music.HalfNote (0, 0)) Music.notePositioner
    let fullNoteInsertionTool =
        Framework.insertionToolTemplate (Music.FullNote (0, 0)) Music.notePositioner

    let music = [halfNoteInsertionTool (5, 5); fullNoteInsertionTool (10, 2)]
    printfn "music: %A" music

test()
