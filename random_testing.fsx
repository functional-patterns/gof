




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