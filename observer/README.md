# Observer


## Intent

Define a one-to-many dependency between objects so that when one object changes state, all its dependents are notified and updated automatically.


## Conclusion

Observer pattern is about some well defined data changing and notifying interested parties. The pattern makes little sense in pure functional context, since there is no changing state (mutable variables).

However, reactive functional programming may be used for the similar purposes. In this case functions operate and return streams of values.


## Examples

N/A
