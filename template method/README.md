# Template Method


### Intent

Define the skeleton of an algorithm in an operation, deferring some steps to subclasses. Template Method lets subclasses redefine certain steps of an algorithm without changing the algorithm's structure.


### Structure

Structure of the Template Method is a function, which takes other functions as parameters. To make concerete functions, the Template Method function is partially applied with correct sub-functions.

~~~~
    type FunctionA :: a -> b
    type FunctionB :: c -> d -> e
    
    templateMethod :: FunctionA -> FunctionB -> x -> y -> z
~~~~

It can execute it's own code between calling the given sub-functions.

~~~~
    templateMethod functionA functionB x y =
        -- do something, call functionA, do something more, call functionB, do final calucaltion
~~~~


## Conclusion

In functional programming Template Method design pattern is higher-order function partially applied with specific sub-functions. It is natural part of functional programming and used basically in every real functional program.

- Fit : Fluent
- Complexity : Trivial


## Examples

[F#](template_method.fsx)
