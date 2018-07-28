///
/// PROTOTYPE
/// 
/// Specify the kinds of objects to create using a prototypical instance, and create new objects by
/// copying this prototype.
/// 

///
/// NOTES
/// 
/// Since all data is immutable in pure functional programming there is no need to clone anything.
/// Cloning is performed in language level when immutable data is modified. That is, a copy of the
/// original value is created with some modified parameters.
/// 
/// Languages like Haskell and F# support modifying of the records with syntactic sugar. For example
/// if 'foo' is a record with some int type field 'size' and 'position' then 'foo' can be cloned
/// with { foo with size = 5 }. This creates a copy of 'foo' with same position, but with size of 5.
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

    let halfNoteInsertionTool =
        Framework.insertionToolTemplate (Music.HalfNote (0, 0)) Music.notePositioner
    let fullNoteInsertionTool =
        Framework.insertionToolTemplate (Music.HalfNote (0, 0)) Music.notePositioner

    halfNoteInsertionTool (5, 5)

let test2() =
    let halfNoteInsertionTool =
        Framework.insertionToolTemplate (Music.HalfNote (0, 0)) Music.notePositioner
    let fullNoteInsertionTool =
        Framework.insertionToolTemplate (Music.HalfNote (0, 0)) Music.notePositioner

    let tools =
        [
            ("half", halfNoteInsertionTool)
            ("full", fullNoteInsertionTool)
        ] |> Map.ofList

    let readPosition () =
        printf "give x:"
        let x = System.Console.ReadLine() |> int
        printf "give y:"
        let y = System.Console.ReadLine() |> int

        Framework.Position (x, y)

    let rec loop elements =
        printf "command:"
        let command = System.Console.ReadLine()
    
        match command with
            | toolCommand when Map.containsKey toolCommand tools ->
                let tool = Map.find toolCommand tools
                let position = readPosition()
                let element = tool position
                loop (element::elements)
            | "quit" ->
                elements
            | _ ->
                printfn "unknown command"
                loop elements

    let elements = loop []
    printfn "notes=%A" elements

test2()
