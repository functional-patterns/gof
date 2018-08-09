# State

Allow an object to alter its behavior when its internal state changes. The object will appear to change its class.


### Structure

Depends of the context. In case of folding through a data structure of `T`s the state passed through each item is following.
~~~~
    data State = State { handler :: State -> T -> State; result :: ResultType }
~~~~

Value of the handler field - which is the function processing the items - has specific function signature. Different handlers process the data and collect the result. State transition happens, when one handler function returns a state value with another handler function.

~~~~
    fooHandler :: State -> T -> State
    barHandler :: State -> T -> State
~~~~


## Conclusion

Pure functional code is stateless. It limits the use cases of the State pattern. However, limited use for it can be found in chained computations. For example folding through a data structure or in state machines. In these cases state consists an altering function, which changes when computation proceeds.

Fit : Artifical
Complexity : Simple


## Examples

[F#](state.fsx)
