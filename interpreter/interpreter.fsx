///
/// INTERPRETER
/// 
/// Given a language, define a represention for its grammar along with an interpreter that uses the
/// representation to interpret sentences in the language.
/// 

///
/// Example
/// 
/// Simple Reversed Polish Notation calculator.
/// 

// Specify grammar with discriminated unions
type Expression = Literal of Literal | Operation of Operation
and Operation = | Plus of Expression * Expression
                | Minus of Expression * Expression
and Literal = int


// Some functions evaluating different types of values
let rec evaluateLiteral number =
    number

and evaluateOperation operation =
    match operation with
        | Plus (left, right) ->
            (evaluateExpression left) + (evaluateExpression right)
        | Minus (left, right) ->
            (evaluateExpression left) - (evaluateExpression right)

and evaluateExpression expression =
    match expression with
        | Literal n ->
            evaluateLiteral n
        | Operation o ->
            evaluateOperation o

let test() =
    // Create expression (2 + 3) - 1
    let one = 1 |> Expression.Literal
    let two = 2 |> Expression.Literal
    let three = 3 |> Expression.Literal

    let sum = Plus (two, three) |> Expression.Operation
    let expression = Minus (sum, one) |> Expression.Operation

    // Calculate the result
    let result = evaluateExpression expression

    printfn "expression = %A" expression
    printfn "result = %A" result

test()
