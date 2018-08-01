




type Calculator = {
    Add : float -> float -> float
    Div : float -> float -> float
    Sub : float -> float -> float
}

let c = { Add = (fun a b -> a + b); Div = (fun a b -> a / b); Sub = (fun a b -> a - b) }


let doCalculations calculator a b =
    printfn "sum %A + %A = %A" a b (calculator.Add a b)
    printfn "sub %A + %A = %A" a b (calculator.Sub a b)
    printfn "div %A + %A = %A" a b (calculator.Div a b)



doCalculations c 10.0 7.0

type Shape = Circle of int | Rectangle of int * int

type Shape with
    member this.Area =
        match this with
            | Circle radius ->
               radius * radius * 3
            | Rectangle (width, height) ->
                width * height

    static member Foo (shape : Shape) =
        shape.Area

let c = Circle 5
let r = Rectangle (6, 2)

c.Area
r.Area



let inline doSomething (s : ^a) : ^b =
    (^a : (static member Foo : ^a -> ^b) s)


doSomething c
doSomething r

































type SteamSystem = Boiler | Pipe | Engine
type ElectricSystem = SolarPanel | Wire | Motor


type SteamSystem with
    static member check (system : SteamSystem list) : bool =
        match system with
            | Boiler::Pipe::Engine::[] -> true
            | _ -> false
    static member label part : string =
        match part with
            | Boiler -> "Boiler"
            | Pipe -> "-Pipe-"
            | Engine -> "Engine"

type ElectricSystem with
    static member check (system : ElectricSystem list) : bool =
        match system with
            | SolarPanel::Wire::Motor::[] -> true
            | _ -> false
    static member label part : string =
        match part with
            | SolarPanel -> "SolarPanel"
            | Wire -> "-Wire-"
            | Motor -> "Motor"
let inline tarkista (system : ^a list) : bool =
    (^a : (static member check : ^a list -> bool) system)

let inline stringify (system : ^a list) : string =
    system
    |> List.map (fun part -> (^a : (static member label : ^a -> string) part))
    |> List.fold (+) ""

let steam = [Boiler;Pipe;Pipe]
let electric = [SolarPanel; Wire; Motor]

let steamFactory part =
    match part with
        | 0 -> Boiler
        | 1 -> Pipe
        | _ -> Engine

let electricFactory part =
    match part with
        | 0 -> SolarPanel
        | 1 -> Wire
        | _ -> Motor



let createSystemTemplate factory =
    let producer = factory 0
    let transmitter = factory 1
    let consumer = factory 2

    [producer; transmitter; consumer]

let test() =
    let steamSystem = createSystemTemplate electricFactory
    let status = tarkista steamSystem
    let composition = stringify steamSystem

    printfn "status of the system %A is %A" composition status



test()




