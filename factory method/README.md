# Factory Method

## Intent

Define an interface for creating an object, but let subclasses decide which class to instantiate. Factory Method lets a class defer instantiation to subclasses. 


## Conclusion

Factory Method in functional programming is a simple function, which takes some input parameters and returns the desired value. Framework code can then use these types, as long as functions to operate with these values are also provided.

There is some options for the signature of the creator function. For example:
~~~~
   1) a -> b -> ^t, where ^t is a generic type
   2) a -> b -> d,  where d is a discriminated union type
   3) a -> b -> e,  where e is arbitrary type
~~~~
In the first case statically resolved type parameters may be used to implement the generic functions (F#) or typeclasses (Haskell).

In the second case basic functions are sufficient, as long as they operatate with all of the discriminated union values.

In the third case set of functions to operate with the specific type 'e' has to be also provided to the framework function, so it can operate with the created values.


## Note

There is no point to create functions with Factory Method. Better way to achieve the same result would be to use a function, which could then be partially applied by the framework functions.


## Example

[F#](factory_method.fsx)
