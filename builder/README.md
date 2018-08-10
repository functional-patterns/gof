# Builder


### Intent

Separate the construction of a complex object from its representation so that the same construction process can create different representations. 


### Structure

Builder is a function template defining a step-by-step construction processes. A concrete builder is created by partially applying the function template. Builder function then optionally chops the given input to be in correct form for the each step. It is also possible to process output of a step function before passing the data for the next step. Essentially the structure is the same as in the [Template Method](../template%20method/README.md).

~~~~
    -- example of builder template
    builderTemplate :: (a -> b) -> (c -> d) -> (e -> f) -> Data -> String
    builderTemplate firstStep secondStep thirdStep input = ...
    
    ...

    -- partially applying different steps to get a concrete builder accepting a data item
    jsonBuilder = jsonBegin jsonContent jsonEnd data
~~~~

In this short listing the output for different concrete builder has the same type (String). If the type of the output product has to variate - for example instead of presenting JSON and XML documents as Strings, but with different types - then typeclasses or similar techniques may be used to handle the actual products.


### Conclusion

Functional builder provides the same benefits as the object-oriented version. It's structure is simple and lightweight.

- Applicability : Natural
- Complexity : Trivial


### Examples

[F#](builder.fsx)
