# Decorator

Attach additional responsibilities to an object dynamically. Decorators provide a flexible alternative to subclassing for extending functionality.


## Conclusion

If pre-function decoration is used then the signature of the decorator's input and output has to be the same as the input of the core function. Also there can be only single input parameter. However, tuple may be used to give actually more parameters. That is:
~~~~
   type coreFunction : (a * b) -> c    =>    type preDecorator : (a * b) - > (a * b)
~~~~

If post-function decoration is used then the signature of the decorator's input and output has to be the same as the output of the core function. That is:
~~~~
   type coreFunction : (a -> b) -> c    =>    type postDecorator : c -> c   
~~~~
Seems that pre-decoration is mapping parameters (input) and post-decoration is mapping the result (output). In both cases it is transparent for the caller if the function is decorated or not.

In pure functional context the post function decorator is more powerful. This is due that in pure functional world everything a function does is the result. If the core function has some fixed values to calculate the result, these cannot be altered by the pre-decoration. Clearly this is not the case with post-decoration, which can alter the result.
 
There is also third kind of decorator, which can operate as pre- and post-decorator. It does both maps the input parameters and also the output. However, these kind of decorators cannot be chained together without additional effort. Signature of this kind of decorator is:
~~~~
   type coreFunction (a -> b) -> c    =>    type fullDecorator : (a - b) -> c
~~~~
