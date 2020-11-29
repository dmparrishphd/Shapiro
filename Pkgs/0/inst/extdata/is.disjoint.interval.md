is.disjoint.interval
====================

Usage
-----

    is.disjoint.interval(x, y)

| Argument | Description |
| -------: | :---------- |
|        x | An interval |
|        y | An interval |

Value
-----

A single `logical` value indicating whether the two interval
arguments are disjoint.

Examples
--------

    is.disjoint.interval(0:1, 1:2)
    # [1] FALSE
    is.disjoint.interval(0:1, c(2, 2))
    # [1] TRUE
    is.disjoint.interval(0:1, c(.3, .7))
    # [1] FALSE
    
