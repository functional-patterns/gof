///
/// STATE
///
/// Allow an object to alter its behavior when its internal state changes. The object will appear to
/// change its class.
///

///
/// CONCLUSION
/// 
/// Functional programming is stateless. It should be no surprise that State design pattern is not
/// well fitted to functional programming.
/// 
/// State machines, state monads and folds may be used to manage states in functional programming.
/// However, none of these feel well suited to address the original intent of the design pattern.
/// 

///
/// EXAMPLE
/// 
/// In this example a state is used to implement a simple message protocol. State of the receiver
/// changes when Open and Close messages are received. If the receiver is waiting for a connection
/// to open, it ignores all Data messages. When connection is established, it handles those.
///

type Message = Open | Data of char | Close


type State = { Continuation : State -> Message -> State; Letters : char list }


let rec listener (state : State) (message : Message) : State =
    match message with
        | Open ->
            { state with Continuation = acceptor }
        | _ ->
            state
and acceptor (state : State) (message : Message) : State =
    match message with
        | Data c ->
            { state with Letters = c::(state.Letters)}
        | Close ->
            { state with Continuation = listener }
        | _ ->
            state

let folder (state : State) (message : Message) =
    state.Continuation state message

let test() =
    let messages = [ Data 'a'; Open; Data 'f'; Data 'o'; Data 'o'; Close; Data '?' ]

    messages
    |> List.fold folder { Continuation = listener; Letters = [] }
    |> fun s -> s.Letters
    |> List.fold (fun s t -> t::s) []
    |> printfn "message=%A"

test()