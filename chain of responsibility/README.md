# Chain of Responsibility


### Intent

Avoid coupling the sender of a request to its receiver by giving more than one object a chance to handle the request. Chain the receiving objects and pass the request along the chain until an object handles it.
 

### Structure

Chain of Responsibility is chain of functions with matching input and output. In case additional context is required then data binding with monads may be used.

~~~~
  regularHandler :: a -> a
  monadicHandler :: a -> m a
~~~~

Common example of monadic handler is a function, which may fail. In this case input is a plain type and output is the same type wrapped inside a Maybe monad. Monadic handler may also be used to add _flag_ value to the output to indicate that no more processing is required.


### Conclusion

In functional programming chaining functions is a common practice. The key difference between the pattern and other function chaining is the purpose. It can be considered to be a restricted version of more general function chaning. Namely the signature of the input and output has to match (at least in monadic level).

Compared to object-oriented version the is one limitation. Since in pure functional programming functions cannot have side effects, the functions cannot have unit as return value. This means that only thing the chain can do is to map the input to some output.

Variation of it has been used for example in [railway oriented programming](https://fsharpforfunandprofit.com/rop/). 

- Applicability : Natural
- Complexity : Medium


### Examples

[F#](chain_of_responsibility.fsx)
