# Visitor


### Intent

Represent an operation to be performed on the elements of an object structure. Visitor lets you define a new operation without changing the classes of the elements on which it operates.



### Structure

Visitor is a function run through a data structure accumulating a result. Visitor for a data structure containing elements of `T` has following signature. 
~~~~
    visitor :: State -> T -> State
    visitor state item =
        -- alter the state according to the received item
~~~~

To visit all the elements in the data structure, a traverser function specific to the data structure is required. Signature for a list structure containing values of `T` is following.

~~~~
    traverser :: (State -> T -> State) -> State -> [T] -> State
    traverser visitor initialState dataStructure =
        -- recursively go through every item on the data structure and pass it to the visitor
~~~~

The same visitor can be used to different data structures. Only the traverser function has to be altered.


### Conclusion

Visitor pattern is commonly known as folding in functional programming. It is combination of two things. A method to traverse through a data structure and a set of functions to perform different operations to each item. For example lists and trees are easy to traverse with recursion.

It is also easy to implement these traversal functions in generic way, so they can operate with any kind of items and take different functions as parameters to do the 'visiting' part of the design pattern. Haskell and F# offer a traverser functions for list by default. 

- Fit : Fluent
- Complexity : Simple


### Examples

[F#](visitor.fsx)
