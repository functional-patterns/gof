# Adapter

## Intent

Convert the interface of a class into another interface clients expect. Adapter lets classes work together that couldn't otherwise because of incompatible interfaces.

## Conclusion

In functional world there is only two things to adapt: data and functions. Since the two things are separated also the adaptation functionality is fine-graned.

- Function adaptation is converting a function signature to another
- Data adaptation is converting data from one form to another

Functional data is always immutable. Thus converting between different formats is done simply with functions. There is no danger that data gets corrupted.

~~~~
  old : divideOld (numerator : float) (denominator : float) : (bool * float)
  new : divideNew (numerator : float) (denominator : float) : float option
~~~~

~~~~
  let divideNew numerator denominator =
    match divideOld numerator denominator with
      | true, value -> Some(value)
      | false, _ -> None
~~~~


## Examples

[F#](adapter.fsx)
