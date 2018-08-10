# Strategy


### Intent

Define a family of algorithms, encapsulate each one, and make them interchangeable. Strategy lets the algorithm vary independently from clients that use it. 



### Structure

Family of functions having the same signature performing similar tasks.

~~~~
    type Strategy = a -> b -> c
    
    fooStrategy :: Strategy
    ...

    barStrategy :: Strategy
    ...
~~~~

Strategy can then be injected to the 'client' function with partial application or passed each time 'client' is used as parameter. It is also possible to pass whole set of strategies to the 'client'. The 'client' then chooses which one to use.

~~~~
    -- client which can be partially applied with a strategy
    clientTemplate :: Strategy -> x -> y -> z
    
    -- client which chooses suitable strategy from the set of available strategies
    clientTemplate :: [Strategy] -> x -> y -> z
    
    -- client which takes strategy as parameter on every call
    client x -> y -> Strategy -> z
~~~~
    

### Conclusion

Startegy design pattern is often used in functional programming. It is trivial concept to any functional language, which has functions as first-class values. Strategy is nothing more than a set of functions having the same interface and performing conceptually same tasks.

- Applicability : Natural
- Complexity : Trivial


### Examples

[F#](strategy.fsx)
