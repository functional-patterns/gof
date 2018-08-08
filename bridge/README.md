# Bridge

## Intent

Decouple an abstraction from its implementation so that the two can vary independently.


## Analysis

### Overview

Fit : Natural
Complexity : Medium


### Structure

The interface part of the Bridge is a _framework_ module with set of function templates and types. Implementation part defines set of functions with matching the signatures expected by the interface functions.

~~~~
  // Interface function to draw polygons expects an implementation function which can draw lines
  drawPolygonTemplate :: (Point -> Point -> Line) -> [Point] -> [Line]
  
  // Implementation function of some other module
  drawLine :: Point -> Point -> Line
~~~~

These functions can then be partially applied with functions of the implementation part of the Bridge.

~~~~
  drawRectangle :: [Point] -> [Line]
  drawRectangle = drawRectangleTemplate drawLine
~~~~


### Conclusion

Bridge pattern may be used for the same purposes in functional programming as it is used in the object-oriented programming.


### Examples

[F#](bridge.fsx)
