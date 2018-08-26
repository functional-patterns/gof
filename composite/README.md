# Composite


### Intent

Compose objects into tree structures to represent part-whole hierarchies. Composite lets clients treat individual objects and compositions of objects uniformly.


### Structure

Composite can be presented as recursive discriminated union type with one of the union types containin more of the union types.

~~~~
  data Shape = Circle Int | Square Int | Composite [Shape]
~~~~

Functions performing operations with the shapes are handling singular shapes as well as composite shapes. User of these functions cannot distinguish if a single shape or composite is uded by a function.

~~~~
  area :: Shape -> Int
  area (Circle radius) = radius * radius * pi
  area (Square size) = size * size
  area (Composite cs) = sum $ map area cs
~~~~

[Visitor](../visitor/README.md) may be used to create a generic way to traverse composite structures and apply functions to contained values.


### Conclusion

In functional programming natural way to implement composite is to use discriminated union. If at least one of the types is a container for the discriminated union type, then it is a composite.

Functions taking the discriminated union types as parameters operate the samy way from the client point of view for single and composite values. Usually these functions use recursion to traverse through the values containing composites.

- Applicability : Natural
- Complexity : Medium


### Examples

[Haskell](composite.hs) [F#](composite.fsx)
