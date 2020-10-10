lcurry
======

Usage
-----

lcurry(f, X)

| Argument | Description                              |
| -------: | :--------------------------------------- |
|      `f` | A function                               |
|      `X` | A `list` of (optionally named) arguments |

Value
-----

A function, with the arguments specified by `X` already determined.

Details
-------

Internally, `X` is combined (as in `c`) with another `list`.
As a result, by way of example,

    lcurry(`+`, 1)
    
returns a function that adds `1` to its argument.

Examples
--------

    lcurry(function(a, b, c) list(A=a, B=b, C=c), list(b=42))(41, 43)
    

References
----------

https://en.wikipedia.org/wiki/Currying
