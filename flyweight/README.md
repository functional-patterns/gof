# Flyweight


## Intent

Use sharing to support large numbers of fine-grained objects efficiently.


## Conclusion

In F# flyweight design pattern (sharing a record) is benefical in terms of memory usage. Sharing improves the memory efficiency in proportion of the shared record size and count. For small records the memory usage of the container nullifies the benefit.

In simple case the flyweight factory can be just prepopulated map of values. However, using pre-populated map does not allow dynamic creation of the flyweights. Alternative for this is to use Memoize pattern with lazy evaluation. Lazy evaluation enables dynamic creation of of the flyweight records from the delivered key. If the data cannot be generated from the key, then it is merely just an id and the map has to be populated statically.

Usual way to implement Memoize pattern in F# is to use mutable data. However, this can be done also with immutable data and lazy evaluation. In Haskell the map type supports lazy evaluation by default. For more information see https://wiki.haskell.org/Memoization.


## Examples

[#F](flyweight.fsx)[Haskell](flyweight.hs)
