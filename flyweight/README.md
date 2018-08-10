# Flyweight


### Intent

Use sharing to support large numbers of fine-grained objects efficiently.


### Structure

Flyweight is a factory function, which returns values by key. It is language specific feature, if the return values are cached to share memory or not.

~~~~
    flyweightFactory :: key -> item
~~~~

If only a small fixed set of flyweight values are required, then a pre-populated map will be sufficient. In other cases lazy evaluation features of underlying functional program can be used to memoize the values. Depending of the implementation this may improve the performance of the functional program in regard to memory usage and speed.

~~~~
    memoize :: (Int -> a) -> (Int -> a)
    memoize f = (map f [0 ..] !!)
~~~~

In Haskell memoize function turns a function to a indexed list of lazyly computed values. Value is computed, when it is required for the first time.


### Conclusion

In F# flyweight design pattern (sharing a record) is benefical in terms of memory usage. Sharing improves the memory efficiency in proportion of the shared record size and count. For small records the memory usage of the container nullifies the benefit.

Usefulness of the Flyweight pattern depends totally of the underlying programming language and it's implementation. This applies also to object-oriented versions. For example one compiler may produce code, which automatically memoizes the computed values. Another may not memoize anything, despite using the pattern.

- Applicability : Artifical
- Complexity : Medium


### Examples

[#F](flyweight.fsx)
[Haskell](flyweight.hs)
