# Command


### Intent

Encapsulate a request as an object, thereby letting you parameterize clients with different requests, queue or log requests, and support undoable operations.


### Structure

Command is a function, which takes a state as an input and returns alterate state. If operations may fail, then monadic value may be used. Note that all of the commands must have the similar signature (either regular or monadic).

~~~~   
  regularFunction :: state -> state
  monadicCommand :: state -> Maybe state
~~~~

It is also possible to combine multiple commands behind a macro command. Signature of that macro template takes a list of commands as the first argument. Thus it can be partially applied with specific commands to create a concrete command.

~~~~
  macroCommandTemplate :: [command] -> state -> state
~~~~

Commands may be passed to _client_ code as a map. Based on some processing client then queries a command from the map and executes it.


### Conclusion

Implementation and use of the Command design pattern in functional programming is straightforward. However, the idea of a command does not fit fluently to pure functional programming. Lack of IO actions and mutability limits it's usability in pure functional programming. 

Since state is handled outside of the pure functional core, the undo/redo actions (specified in the original intent) cannot be added directly to the specific commands. However, since command functions take and return states the caller may easily implement a simple stack to store all of the states. This allows trivial undo/redo implementation.


- Applicability : Artifical
- Complexity : Trivial


### Examples

[F#](command.fsx)
