type FunHolder<'Arg, 'Result>() =
    // F# 3.0
    static member val Definition =
        Unchecked.defaultof<'Arg -> 'Result> with get, set

FunHolder.Definition <- fun x -> x * 2
FunHolder.Definition <- fun (x:string) -> x + x
FunHolder.Definition <- fun () -> (), ()

let twice<'a, 'b> = FunHolder<'a, 'b>.Definition

let x : int         = twice 10  // => 20
let y : string      = twice "X" // => "XX"
let z : unit * unit = twice ()  // => (null, null)


let twicer a = twice a

twicer 5


type Next<'a> = 'a -> 'a

let foo : Next<int> = fun a -> a + 1
let bar : Next<float> = fun a -> a + 0.1

type Iterable<'a> =
    static member inline Next(a : int) = a + 1
    static member inline Next(a : float) = a + 0.1

type Foo = Iterable<float>


