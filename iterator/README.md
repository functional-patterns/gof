# Iterator


### Intent

Provide a way to access the elements of an aggregate object sequentially without exposing its underlying representation.



### Structure

Structure of the pattern varies depending of the underlying data structure. Set of functions operating with a particular data structure `Data` containing values of `T` is following.

~~~~
    forward :: (Data, [Crumb]) -> (Data, [Crumb])
    backward :: (Data, [Crumb]) -> (Data, [Crumb])
    get :: (Data, [Crumb]) -> T
    set :: (Data, [Crumb]) -> T -> (Data, [Crumb])
~~~~

Idea is to leave 'crumbs' behind while traversing the data structure. These pieces of information can be then used to reconstruct the data when traversing or when modifying the data structure. The underlying idea is called [Zipper](https://en.wikipedia.org/wiki/Zipper_(data_structure)).


## Conclusion

Implementing traversal through list or tree structure is easy. However, creating an iterator implementation is more challenging task in functional programming. Since the iterator cannot change the state, it must use different method to do the step-by-step iteration. Zippers are well known method in functional paradigm. When iterating forward in a data structure, data is stored to restore the previous state. 

Concept of Iterator is not well suited to functional programming. Usually data structures are traversed in single run instead of step-by-step method. While implementation of a recursive tree traverser is just a few lines of code, creating an iterator implementation is way more complex.

- Fit : Artifical
- Complexity : Complex


## Examples

[F#](iterator.fsx)
