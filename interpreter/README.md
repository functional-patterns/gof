# Interpreter


### Original Intent

Given a language, define a represention for its grammar along with an interpreter that uses the representation to interpret sentences in the language.


### Structure

Discriminated unions are used to define the language. 

~~~~
    data Expression = Literal Literal | Operation Operation
    data Literal = One | Two | Three
    data Operation = Plus Expression Expression | Minus Expression Expression
~~~~

Function to interpret the language specified by the discriminated unions works in recursive manner. Recursion stops, when the terminating literal is met.

~~~~
    evaluateExpression :: Expression -> Int
    evaluateExpression (Literal literal) = evaluateLiteral literal
    evaluateExpression (Operation operation) = evaluateLiteral operation
    
    evaluateOperation :: Operation -> Int
    evaluateOperation ...
    
    evaluateLiteral :: Literal -> Int
    // end of recursion
~~~~
    

## Conclusion

In functional programming Interpreter is easy to implement and useful. Discriminated unions are ideal to present the hiararcial grammar.


## Examples

[F#](interpreter.fsx)
