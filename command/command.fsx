///
/// COMMAND
/// 
/// Encapsulate a request as an object, thereby letting you parameterize clients with different
/// requests, queue or log requests, and support undoable operations.
/// 

///
/// Applicability
/// 
/// Use the Command pattern when you want to
/// - parameterize objects by an action to perform, as MenuItem objects did above. You can express
///   such parameterization in a procedural language with a callback function, that is, a function
///   that's registered somewhere to be called at a later point. Commands are an object-oriented
///   replacement for callbacks.
/// - specify, queue, and execute requests at different times. A Command object can have a lifetime
///   independent of the original request. If the receiver of a request can be represented in an
///   address space-independent way, then you can transfer a command object for the request to a
///   different process and fulfill the request there.
/// - support undo. The Command's Execute operation can store state for reversing its effects in the
///   command itself. The Command interface must have an added Unexecute operation that reverses the
///   effects of a previous call to Execute. Executed commands are stored in a history list.
///   Unlimited-level undo and redo is achieved by traversing this list backwards and forwards
///   calling Unexecute and Execute, respectively.
/// - support logging changes so that they can be reapplied in case of a system crash. By augmenting
///   the Command interface with load and store operations, you can keep a persistent log of
///   changes. Recovering from a crash involves reloading logged commands from disk and reexecuting
///   them with the Execute operation.
/// - structure a system around high-level operations built on primitives operations. Such a
///   structure is common in information systems that support transactions. A transaction
///   encapsulates a set of changes to data. The Command pattern offers a way to model transactions.
///   Commands have a common interface, letting you invoke all transactions the same way. The
///   pattern also makes it easy to extend the system with new transactions.
/// 

///
/// REMARKS
/// 
/// Seems that commands are usefull only when there is some global state they are changing.
/// 
/// Supporting undo/redo functionality some mutable state should be introduced. This can be only
/// done in impure context.
/// 
///
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
/// Example
/// 
/// In this example a simple calculator featuring +, -, *, - and abs operations is implemented with
/// regular commands. More adcanced command, diff, is implemented as macro command.
/// 
type State = int list
type Command = State -> State option

let (>>=) a f =
    match a with
        | Some a -> f a
        | _ -> None

let addCommand state =
    match state with
        | a::b::tail ->
            Some ((a + b)::tail)
        | _ -> None

let subCommand state =
    match state with
        | a::b::tail ->
            Some ((a - b)::tail)
        | _ -> None

let mulCommand state =
    match state with
        | a::b::tail ->
            Some ((a * b)::tail)
        | _ -> None

let macroCommand commands state =
    List.fold (>>=) (Some state) commands
    

let divCommand state =
    match state with
        | _::0::_ ->
            None
        | a::b::tail ->
            Some ((a / b)::tail)
        | _ -> None

let absCommand state =
    match state with
        | a::tail ->
            Some ((if a > 0 then a else -a)::tail)
        | _ -> None

let test() =
    let diffCommand = macroCommand [ subCommand; absCommand ];

    let operations = [ ("+", addCommand)
                       ("-", subCommand)
                       ("*", mulCommand)
                       ("/", divCommand)
                       ("abs", absCommand)
                       ("diff", diffCommand) ] |> Map.ofList


    let rec run state =
        printfn "state is: %A" state
        printf "command> "
        let token = System.Console.ReadLine()

        match state with
            | Some state ->
                if Map.containsKey token operations
                then run ((Map.find token operations) state)
                elif (fst (System.Int32.TryParse token))
                then run (Some ((int token)::state))
            | None ->
                ()

    run (Some [])

test()
