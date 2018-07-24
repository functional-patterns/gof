


type Shape = Square of Square | Circle of Circle
and Square = { Width : float; Height : float }
and Circle = { Radius : float }


let inline squareArea square =
    square.Width * square.Height

let inline circleArea circle =
    3.14 * circle.Radius

type Circle with
    member this.Area =
        3.14 * this.Radius * this.Radius

type Square with
    member this.Area =
        this.Width * this.Height

let c = { Radius = 1.0 }
let r = { Width = 2.0; Height = 3.0 }

c.Area
r.Area

let inline calculateArea (shape : ^T) : float =
    shape.Area



type A = { thing: int }
    with static member show a = sprintf "foo %A" a
type B = { label: string }
    with static member show b = sprintf "bar %A" b

let inline show (x:^t) =
    (^t: (static member show: ^t -> string) (x))

{ thing = 98 } |> show |> printfn "%s"
{ label = "Car" } |> show |> printfn "%s"





let foolaa a = a + 1


let moolaa = Option.map foolaa



let (>>=)  (a : 'a) (f : 'a -> 'b)  = f a

let add a b = a + b


10 |> (add 2) >>= (add 5)


let (>>=) m f = 
    printfn "expression is %A" m
    f m

let loggingWorkflow =   
    1 >>= (+) 2 >>= (*) 42 >>= id

loggingWorkflow




let foo (a : int) = float a + 1.0

let bind (a : float) (f : int -> float)  : float =
    let temp = int a
    f temp

let (>>=) = bind


let convert (a : int) : float = float a
10.0 >>= (convert) |> (+) 5.0














type WorkflowBuilder() =
    member this.Bind(m : int option, f : int -> int option) : int option =
        match m with
            | Some x -> f x
            | _ -> None

    member this.Return(x : int) =
        Some x


let workflow = new WorkflowBuilder()

let strToInt str =
       match System.Int32.TryParse str with
        | true, i -> Some i
        | _ -> None

let stringAddWorkflow x y z = 
    workflow 
        {
        let! a = strToInt x
        let! b = strToInt y
        let! c = strToInt z
        return a + b + c
        }    

// test
let good = stringAddWorkflow "12" "3" "2"
let bad = stringAddWorkflow "12" "2" "foo"





type FloaterBuilder() =
    member this.Bind(m : float, f : int -> float) : float =
        let i = int m
        f i
    
    member this.Return(x : int) =
        float x

let floaterBuilder = new FloaterBuilder()


let pow a b =
    let fa = float a
    let fb = float b

    System.Math.Pow(fa, fb)


let workflow a b c d = 
    floaterBuilder {
        let! s0 = pow a (float b)
        let! s1 = pow s0 (float c)
        let! s2 = pow s1 (float d)
        return s2
    }

let test = workflow 2 2 1 1

