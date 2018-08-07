# Composite


## Intent

Compose objects into tree structures to represent part-whole hierarchies. Composite lets clients treat individual objects and compositions of objects uniformly.


## Conclusion

In functional programming natural way to implement composite is to use discriminated union. If at least one of the types is a container for the discriminated union type, then it is a composite.

Functions taking the discriminated union types as parameters operate the samy way from the client point of view for single and composite values. Usually these functions use recursion to traverse through the values containing composites.

Visitor pattern may be used to create a generic way to traverse composite structures and apply functions to contained values. 
