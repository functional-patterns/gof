# Factory Method


### Intent

Define an interface for creating an object, but let subclasses decide which class to instantiate. Factory Method lets a class defer instantiation to subclasses. 


### Structure

Factory method is a function accepting one or more parameters and returning a value. The type of the returned value must support all the functionality expected by the 'client' code.

There are two options to add function support for the instantiated type.

1. Returned type is defined to be an instance of specific typeclass
~~~~
    shapeFactory :: (Shape shape) => Int -> Shape
    shapeFactory size = ...
~~~~

2. Returned type is member of a specific discriminated union
~~~~
    data Shape = Circle Int | Rectangle Int Int
    shapeFactory :: Int -> Shape
~~~~


### Conclusion

Original intent of the Factory Method is not well suited for functional programming. Concept of creating abstract data feels unnatural. Creating functions is also useless, since instead of passing abstract factory as an argument the concrete functions could be passed directly.

- Fit : Artifical
- Complexity : Medium


### Example

[F#](factory_method.fsx)
