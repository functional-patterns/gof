# Decorator


### Intent

Attach additional responsibilities to an object dynamically. Decorators provide a flexible alternative to subclassing for extending functionality.


### Structure

Functional decoration applies functions functions. Decorating a function may be done in multiple ways. However, important part is that signature of the function remains the same.

~~~~
    coreFunction :: a -> b -> c
    decoratedFunction :: a -> b -> c
~~~~

Decorator can be placed before, after or both sides of the core function. 

Pre-decorator modifies the parameters before those are passed to the core function.
~~~~
    PreDecorator :: (a -> b) -> (a, b)
~~~~

Post-decorator modifies the output of the core function before passing it out.
~~~~
    PostDecorator :: c -> c
~~~~

To make it easy to compose different decorators some bind functions can be defined.
~~~~
    pre :: (a, b) -> (a -b -> c) -> c
    pre ab f = let (a, b) = ab in f a b
    
    post :: c -> (c -> c) -> c
    post c f = f c
~~~~

With these bind functions arbitrary decorated function can be created from core function, pre-decorators and post-decorators.

~~~~
    decorated :: a -> b -> c
    decorated a b = (preIncreaser a b) `pre` preMaximizer `pre` coreFunction `post` postSquarer
~~~~
   

## Conclusion

Chaining different functions together, is the natural way to compose functional programs. Decorator is merely a restricted version of more general function chaning. It keeps the signature of the function the same. Structure of the design pattern is really simple and flexible. Compared to the object-oriented version, less code is required since

In some extend it may be used to same purposes as the object-oriented counterpart. Sometimes the object-oriented version is used to generate side-effects - for example logging - which cannot be done in pure functional programming without altering the result of the core function. 

- Fit : Fluent
- Complexity : Trivial


## Examples

[F#](decorator.fsx)
