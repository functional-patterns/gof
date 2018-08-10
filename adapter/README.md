# Adapter

### Intent

Convert the interface of a class into another interface clients expect. Adapter lets classes work together that couldn't otherwise because of incompatible interfaces.


### Structure

Adapting a data values from one format to another requires conversion functions from and back.

~~~~
  type Original = ...
  type Adapted = ...
  
  fromOriginal :: Original -> Adapted
  toOriginal :: Adapted -> Original
~~~~

Adapting functions is changing the signature of the function.

~~~~
  originalFunction :: Original -> Int
  
  adaptedFunction :: Adapted -> Int
  adaptedFunction adapted = originalFunction (toOriginal adapted)
~~~~
  

### Conclusion

Adapter pattern is very useful and common in functional programming. It's structure is so trivial, that it is not concidered to be a design pattern at all in functional programming.

- Applicability : Natural
- Complexity : Trivial


### Examples

[F#](adapter.fsx)
