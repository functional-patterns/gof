# Prototype


### Intent

Specify the kinds of objects to create using a prototypical instance, and create new objects by copying this prototype.


### Structure

Core part of the Prototype pattern is a function, which can be partially applied with a prototype and a function modifying the injected prototype. Since all data is immutable, the modifying is creation a new value with some altered fields.

~~~~
    clonerTemplate :: Prototype -> (Prototype -> a -> Prototype) -> a -> Prototype
    clonerTemplate prototype function parameter = function prototype parameter
    
    cloner parameter = clonerTemplate someProptotype someFunction
~~~~

Depending of the given parameter, a different clone of the original prototype is created.


### Conclusion

Prototype does not feel natural in functional programming. Functional languages - like Haskell and F# - support altering the record types with simpler syntax. There is little value to do the same with a specific pattern.

- Applicability : Artifical
- Complexity : Trivial


### Examples

[Haskell](prototype.hs) [F#](prototype.fsx)
