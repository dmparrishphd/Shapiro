det2
====

Usage
-----

    det2(m)
    
| Argument | Description             |
| -------: | :---------------------- |
|      `m` |  a 2 x 2 numeric matrix |
    
Value
-----

The determinant of the argument.

Details
-------

For integer matrices, **fails on overflow**.
        
Originally intended to produce an exact result with integer matrices (unlike the built-in `det`),
provided that overflow does not occur at any(intermediate) stage.
