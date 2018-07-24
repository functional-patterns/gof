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

type Expression = Number of Number | Operation of Operation
and Number = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
and Operation = Plus of Expression * Expression | Minus of Expression * Expression


let expression : Expression = Number Four


let foo = Plus ((Number Zero), Number One)


let rec evaluateNumber number =
    match number with
        | Zero -> 0
        | One -> 1
        | Two -> 2
        | Three -> 3
        | Four -> 4
        | Five -> 5
        | Six -> 6
        | Seven -> 7
        | Eight -> 8
        | Nine -> 9

and evaluateOperation operation =
    match operation with
        | Plus (left, right) ->
            (evaluateExpression left) + (evaluateExpression right)
        | _ -> 0

and evaluateExpression expression =
    match expression with
        | Number n ->
            evaluateNumber n
        | Operation o ->
            evaluateOperation o

let test = Operation (Plus (Number One, Number Two))

evaluateExpression test


