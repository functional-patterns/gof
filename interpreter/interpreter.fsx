///
/// INTERPRETER
/// 
/// Given a language, define a represention for its grammar along with an interpreter that uses the
/// representation to interpret sentences in the language.
/// 

///
/// Example
/// 
/// Simple Reversed Polish Notation calculator
/// 

type Expression = Number of Number | Operation of Operation | Empty
and Number = int
and Operation = | Plus of Expression * Expression
                | Minus of Expression * Expression
                | Factorial of Expression


let rec evaluateNumber number =
    number

and evaluateOperation operation =
    match operation with
        | Plus (left, right) ->
            (evaluateExpression left) + (evaluateExpression right)
        | Minus (left, right) ->
            (evaluateExpression left) - (evaluateExpression right)
        | Factorial a ->
            let rec factorial n = match n with 1 -> 1 | n -> n * factorial (n - 1)
            factorial (evaluateExpression a)

and evaluateExpression expression =
    match expression with
        | Number n ->
            evaluateNumber n
        | Operation o ->
            evaluateOperation o


let rec parse input stack =
    match input with
        | [] ->
            stack
        | token::input ->
            match token with
                | "+" ->
                    match stack with
                        a::b::stack ->
                            let e = Expression.Operation (Operation.Plus (a, b))
                            parse input (e::stack)
                | "-" ->
                    match stack with
                        a::b::stack -> 
                            let e = Expression.Operation (Operation.Minus (a, b))
                            parse input (e::stack)
                | "!" ->
                    match stack with
                        a::stack ->
                            let e = Expression.Operation (Operation.Factorial a)
                            parse input (e::stack)
                | number ->
                    let n = System.Int32.Parse number
                    let e = Expression.Number n
                    parse input (e::stack)


let test() =
    // Calculate expression 100 + 10 - 200
    let tester = string("100 10 + 200 -").Split ' ' |> List.ofArray
    let expression = List.head (parse tester [])
    printfn "expression = %A" expression
    evaluateExpression expression

test()