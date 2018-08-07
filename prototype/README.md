# Prototype


## Intent

Specify the kinds of objects to create using a prototypical instance, and create new objects by copying this prototype.


## Conclusion

Since all data is immutable in pure functional programming there is no need to clone anything. Cloning is performed in language level when immutable data is modified. That is, a copy of the original value is created with some modified parameters.

Languages like Haskell and F# support modifying of the records with syntactic sugar. For example:

~~~~
    type Circle = { Radius : int; X : int; Y : int }
    
    let original = { Radius = 5; X = 1; Y = 13 }
    let copy = { foo with Radius = 6 }
~~~~

This code snipped creates a copy of value 'original' as 'copy' with increased radius.


## Examples

[F#](prototype.fsx)

