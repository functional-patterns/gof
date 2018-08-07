# Visitor

## Intent

Represent an operation to be performed on the elements of an object structure. Visitor lets you define a new operation without changing the classes of the elements on which it operates.


## Conclusion
 
In functional programming Visitor is combination of two things. A method to traverse through a data structure and a set of functions to perform different operations to each item. For example lists and trees are easy to traverse with recursion.

It is also easy to implement these traversal functions in generic way, so they can operate with any kind of items and take different functions as parameters to do the 'visiting' part of the design pattern.


## Examples

[F#](visitor.fsx)
