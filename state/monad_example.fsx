///
/// STATE MONAD
/// 
type State<'st,'a> =
    | Ok of  'a * 'st
    | Error of ErrorState
and ErrorState = string
and StateMonadBuilder() =
    member b.Return(x) = fun s -> Ok (x, s)
    member b.ReturnFrom(x) = x
    member b.Error msg = fun _ -> Error msg
    member b.Bind(p, rest) =
        fun state ->
                 let result = p state in
                 match result with
                 | Ok (value,state2) -> (rest value) state2
                 | Error msg -> Error msg  
 
    member b.Get () = fun state -> Ok (state, state)
    member b.Put s = fun _ -> Ok ((), s)
 
let state = StateMonadBuilder()








///
/// Core functions
/// 
let moveUp () =
    state {
        let! pos = state.Get()
        return! state.Put(fst pos, snd pos - 1)
    }
 
let moveRight () =
    state {
        let! pos = state.Get()
        return! state.Put(fst pos + 1, snd pos)
    }
 
let test pos1 pos2 = fst pos1 = fst pos2 || snd pos1 = snd pos2
 
let moveUpAndTest testPos =
    state {
        do! moveUp()
        let! pos = state.Get()
        return test pos testPos
    }
 
 ///
 /// Executing
 /// 
let run () =
    state {
        do! moveUp()
        let! res =  moveUpAndTest (5, 4)
        if res
        then
            do! moveRight ()
        else
            do! moveUp()
    }
 
let result = match run () (5, 5) with
             | Ok (_, pos) -> pos
             | Error _ -> (0, 0)
