
# Abstract Factory

 Provide an interface for creating families of related or dependent objects without specifying their concrete classes.


## Conclusion

In functional programming related or dependent values are created with discriminated unions. Related functions working on the specific types has to be provided also. This enables 3rd party modules to be attached to the program easily.

If new values under the type system has to be added or removed, the concequences are similar as in the object-oriented version. That is, the existing type hierarchies must be refactored to match the new hierarchies.

Lack of function polymorphism in F# makes it a bit trickier to provide the functions working. with the factory created types. However, these problems can be overcome with statically resolved type parameters. Another way would be to partially apply the framework functions with the specific module functions. In Haskell typeclasses can be used instead.

## Note

Instead of creating separate type hierarchies (Window, ScrollBar in the original GoF exsample) it is easier to just create discriminated unions with all the related or dependant values. It is also possible to create separate typehierarchies, but this way feels less functional and more tedious.

## Note

Instead of using a factory record type, plain functions could be applied to the framework function directly.
