type FunHolder<'Arg, 'Result>() =
    // F# 3.0
    //static member val Definition =
    //    Unchecked.defaultof<'Arg -> 'Result> with get, set
    // F# 2.0
    static let mutable definition = Unchecked.defaultof<'Arg -> 'Result>
    static member Definition with get()  = definition
                             and  set(x) = definition <- x

FunHolder.Definition <- fun x -> x * 2
FunHolder.Definition <- fun (x:string) -> x + x
FunHolder.Definition <- fun () -> (), ()

let twice<'a, 'b> = FunHolder<'a, 'b>.Definition

let x : int         = twice 10  // => 20
let y : string      = twice "X" // => "XX"
let z : unit * unit = twice ()  // => (null, null)



