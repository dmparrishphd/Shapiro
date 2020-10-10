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

Examples
--------

    lcurry(`+`, 1)(42)
    # [1] 43

References
----------

https://en.wikipedia.org/wiki/Currying
