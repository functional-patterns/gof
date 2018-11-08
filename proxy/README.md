# Proxy


### Intent

Provide a surrogate or placeholder for another object to control access to it.


### Structure

Proxy is a function, which has the same signature as the function it surrogates.

~~~~
    coreFunction :: a -> b -> c
    proxyFunction :: a -> b -> c
~~~~

Proxy can be created from a function template with partial application or using closures.

~~~~
    proxyFunctionTemplate :: (a -> b -> c) -> a -> b -> c
    proxyFunctionTemplate coreFunction a b =
        -- do something before calling the coreFunction
        let c = coreFunction a b
        -- do something after calling the core function
~~~~

Depending of the proxyFunctionTemplate implementation, proxy can do various tasks. For example restricting access or caching results.


### Conclusion

Proxy pattern fits well to functional programming. It can perform the same tasks as it's object oriented counterpart. Including caching, access restriction and altering the result. While the basic structure is simple, the concrete proxy implementation can be arbitrary complext.

Proxy and  [Decorator](../decorator/README.md)  have the same structure. Only difference between the patterns in functional programming is the purpose.

- Applicability : Natural
- Complexity : Trivial


### Examples

[Haskell - Efficient lookup](proxy_1.hs)  
[Haskell - Access policy](proxy_2.hs)  
[F#](proxy.fsx)
