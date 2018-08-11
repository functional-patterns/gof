///
/// COMMAND
/// 
/// Encapsulate a request as an object, thereby letting you parameterize clients with different
/// requests, queue or log requests, and support undoable operations.
/// 

///
/// Example
/// 
/// In this example program provides some pre-determined inputs, which are mapped to corresponding
/// commands. Single command may move a circle to any of the four main directions. Macro command is
/// used to execute multiple base commands sequantially.
/// 

// State of the application is the position and radius of a circle
type Circle = { X : int; Y : int; Radius : int }

// Inputs to move the circle (North, South, ..., South West)
type Input = N | S | E | W | NE | NW | SE | SW

// Specify some commands...
let moveLeft circle = { circle with X = circle.X - 1 }
let moveRight circle = { circle with X = circle.X + 1 }
let moveUp circle = { circle with Y = circle.Y + 1 }
let moveDown circle = { circle with Y = circle.Y - 1 }
let macroTemplate commands (circle : Circle) =
    commands |> List.fold (fun circle command -> command circle) circle

// ... use macro command to implement diagonal shifts with basic commands
let moveUpLeft circle = macroTemplate [moveUp; moveLeft] circle
let moveUpRight circle = macroTemplate [moveUp; moveRight] circle
let moveDownLeft circle = macroTemplate [moveDown; moveLeft] circle
let moveDownRight circle = macroTemplate [moveDown; moveRight] circle


let commands = [
            (N, moveUp)
            (S, moveDown)
            (E, moveLeft)
            (W, moveRight)
            (NE, moveUpLeft)
            (NW, moveUpRight)
            (SE, moveDownLeft)
            (SW, moveDownRight) ] |> Map.ofList

let test() =
    // Simulate user inputs or program parameters
    let inputs = [ N; N; N; E; SE ]

    // Create initial state
    let initialState = { X = 0; Y = 0; Radius = 5 }
    
    // Command executor (calls command function with the current state)
    let handler state input = 
        let command = Map.find input commands
        command state

    // Run all the inputs through the initial state
    let finalState = List.fold handler initialState inputs

    printfn "initial state: %A" initialState
    printfn "final state: %A" finalState

test()
