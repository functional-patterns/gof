# Command

Encapsulate a request as an object, thereby letting you parameterize clients with different requests, queue or log requests, and support undoable operations.


## Conclusion

Command is just a regular function in functional programming. Signature of the function may be
~~~~   
     state -> state    or    state -> state option
~~~~ 
This depends if commands always succeed or not. Since functions cannot alter any state, the state has to be passed to the function and altered state has to be returned.

Macro command is easy to implement with partial application. After that any list of commands may be used to create macro command (that is, a command executing multiple commands).

Since state is handled outside of the pure functional core, the undo/redo actions cannot be added directly to the specific commands. However, since command functions take and return states the caller may easily implement a simple stack to store all of the states. This allows trivial undo/redo implementation.


## Note

Seems that commands are usefull only when there is some global state they are changing.


## Examples

[F#](command.fsx)
