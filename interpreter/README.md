# Interpreter


### Intent

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
    evaluateOperation :: Operation -> Int
    evaluateLiteral :: Literal -> Int
~~~~
    

## Conclusion

Defining and evaluating arbitrary languages in functional way is natural. Discriminated unions are lightweight syntax to define the language. Recursive functions are used to evaluate the expressions.

Functional version is more compact and straightforward than object-oriented version. There is no need for classes or mutable data in the design pattern.

- Fit : Fluent
- Complexity : Medium


## Examples

[F#](interpreter.fsx) [Haskell](interpreter.hs)
