///
/// COMMAND
/// 
/// Encapsulate a request as an object, thereby letting you parameterize clients with different
/// requests, queue or log requests, and support undoable operations.
/// 

///
/// CONCLUSION
/// 
/// Command is just a regular function in functional programming. Signature of the function may be
///   
///     state -> state    or    state -> state option
/// 
/// This depends if commands always succeed or not. Since functions cannot alter any state, the
/// state has to be passed to the function and altered state has to be returned.
/// 
/// Macro command is easy to implement with partial application. After that any list of commands
/// may be used to create macro command (that is, a command executing multiple commands).
/// 
/// Since state is handled outside of the pure functional core, the undo/redo actions cannot
/// be added directly to the specific commands. However, since command functions take and return
/// states the caller may easily implement a simple stack to store all of the states. This allows
/// trivial undo/redo implementation.
/// 

///
/// REMARKS
/// 
/// Seems that commands are usefull only when there is some global state they are changing.
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
