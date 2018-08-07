# Iterator


## Intent

Provide a way to access the elements of an aggregate object sequentially without exposing its underlying representation.


## Conclusion

Implementing traversal through list or tree structure is easy. However, creating an iterator implementation is more challenging task in functional programming. Since the iterator cannot change the state, it must use different method to do the step-by-step iteration.

Zippers are well known method in functional paradigm. When iterating forward in a data structure, the zipper is storing information to get back. It also enables "changing" values of
the data structure by creating new data on demand.


## Examples

[F#](iterator.fsx)
