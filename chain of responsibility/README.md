# Chain of Responsibility


## Intent

Avoid coupling the sender of a request to its receiver by giving more than one object a chance to handle the request. Chain the receiving objects and pass the request along the chain until an object handles it.
 

## Conclusion

In functional programming functions can be chained easily. Monad is used to add some result value, which then may be accumulated by the functions in the chain.

Like in the object-oriented counter part, the functions need to have same signature to make the chaining process easy.


## Note

In object-oriented version the handlers are objects. In functional programming the handlers are functions. Since pure functional programming cannot have side effects, the Chain of Responsibility cannot alter any state or do I/O operations. Thus in functional version the functionality is limited to alter the output.


## Examples

[F#](chain_of_responsibility.fsx)
