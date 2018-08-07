# GoF

GoF design patterns in pure functional light


## Patterns

These are the 23 original GoF design patterns, grouped in three categories by the main purpose.


### Creational

- [Abstract Factory](abstract%20factory/README.md) pattern groups object factories that have a common theme
- [Builder](builder/README.md) pattern constructs complex objects by separating construction and representation
- [Factory Method](factory%20method/README.md) pattern creates objects without specifying the exact class to create
- [Prototype](prototype/README.md) pattern creates objects by cloning an existing object
- [Singleton](singleton/README.md) pattern restricts object creation for a class to only one instance


### Structural

- [Adapter](adapter/README.md) allows classes with incompatible interfaces to work together by wrapping its own interface around that of an already existing class
- [Bridge](bridge/README.md) decouples an abstraction from its implementation so that the two can vary independently
- [Composite](composite/README.md) composes zero-or-more similar objects so that they can be manipulated as one object
- [Decorator](decorator/README.md) dynamically adds/overrides behaviour in an existing method of an object
- [Facade](facade/README.md) provides a simplified interface to a large body of code
- [Flyweight](flyweight/README.md) reduces the cost of creating and manipulating a large number of similar objects
- [Proxy](proxy/README.md) provides a placeholder for another object to control access, reduce cost, and reduce complexity


### Behavioral

- [Chain of Responsibility](chain%20of%20responsibility/README.md) delegates commands to a chain of processing objects
- [Command](command/README.md) creates objects which encapsulate actions and parameters
- [Interpreter](interpreter/README.md) implements a specialized language
- [Iterator](iterator/README.md) accesses the elements of an object sequentially without exposing its underlying representation
- [Mediator](mediator/README.md) allows loose coupling between classes by being the only class that has detailed knowledge of their methods
- [Memento](memento/README.md) provides the ability to restore an object to its previous state (undo)
- [Observer](observer/README.md) is a publish/subscribe pattern which allows a number of observer objects to see an event
- [State](state/README.md) allows an object to alter its behavior when its internal state changes
- [Strategy](strategy/README.md) allows one of a family of algorithms to be selected on-the-fly at runtime
- [Template Method](template%20method/README.md) defines the skeleton of an algorithm as an abstract class, allowing its subclasses to provide concrete behavior
- [Visitor](visitor/README.md) separates an algorithm from an object structure by moving the hierarchy of methods into one object


## Classification

Each of the patterns is classified with the criteria described here.


### Meaningful

If the _intention_ of the design pattern is meaningful in pure functional programming. For example Observer design pattern is used to notify changes in the subject object. Since immutability is one of the key concepts in functional programming, there is not much use for the design pattern.

If answer for this question is "No", then rest of the classification criterias are omitted. The pattern simply does not work in functional programming.

Value: Yes | No


### Complexity

Some design patterns are very trivial to implement in functional programming. For example Template Method is just a function which is then partially applied to do something usefull. On the other hand Iterator is way more complex to implement and understand.

Value: Simple | Medium | Complex


### Structure

Values: 


### Usefulness

If something can be done, it does not mean it should be done. Each design pattern is evaluated if it makes sense to be used in functional programming.

Value: Minor | Medium | Major



## Analysis


### Applicability

If the _intention_ of the design pattern is meaningful in pure functional programming. For example Observer design pattern is used to notify changes in the subject object. Since immutability is one of the key concepts in functional programming, there is not much use for the design pattern.

On the otherhand State design pattern deals with immutable state. At first glance it may look totally useless in functional programming. However when analysed further, the meaning of the pattern is to get rid of the if-then-else structure and replace it with an object. In functional programming there is also a lot of concepts involving a concept of 'state'. On of these is the state during a fold operation. The idea behind the State design pattern can be directly applied to a complex fold operation.

If answer for this question is "No", then rest of the anylises criterias are omitted. The pattern simply does not work in functional programming.

Value: Yes | No


### Usefulness

If something can be done, it does not mean it should be done. Each design pattern is evaluated if it makes sense to be used in functional programming.

Value: Minor | Medium | Major


### Structure

Some design patterns are very trivial to implement in functional programming. For example Template Method is just a function which is then partially applied to do something usefull. On the other hand Iterator is way more complex to implement and understand.

Value: Simple | Medium | Complex


### Conclusion

Short summary of the all above things with optional notes and remarks.


### Examples

If answer to the "applicability" was 'Yes', then a F# example is presented. Some of the examples are also implemented in Haskell. In these cases it is examined if typeclasses or other features missing from F# do any difference.











