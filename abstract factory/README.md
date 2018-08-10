
# Abstract Factory


### Intent

Provide an interface for creating families of related or dependent objects without specifying their concrete classes.


### Structure

Discriminated union is straightforward way to present values of a specific product family. Functions required to operate with the specific product family are made available to 'framework' code by typeclasses, statically resolved type parameters or by injecting functions with partial application.

The actual 'factory' item is a set of functions each creating a specific product. These may be combined to a 'Factory' record so they all can be passed together.


### Conclusion

Abstract Factory is applicable also in functional programming. Discriminated unions are a natural way to enforce that only compatible data is used by the 'framework' code. Lack of functional polymorphism makes the pattern a bit cumbersome in F#. However, other techniques may be used to overcome this obstacle. In Haskell typeclasses are helpful.

- Applicably : Artifical
- Complexity : Medium


### Examples

[F#](abstract_factory.fsx)
[Haskell](abstract_factory.hs)
