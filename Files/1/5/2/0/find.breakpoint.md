find.breakpoint
===============

Usage
-----

find.breakpoint(X, x)

| Argument | Description                                      |
| -------: | :----------------------------------------------- |
|        X | A `numeric vector` of strictly increasing values |
|        x | a single value comparable with members of `X`    |

Value
-----

The index of the greatest member of `X` not greater than `x`

Warnings
--------

Expect strange results if `X` is not as described.

Example
-------

find.breakpoint(as.Date(c("1985-11-01", "1985-12-01")), as.Date("1985-11-05"))
# [1] 1
find.breakpoint(as.Date(c("1985-11-05", "1985-12-01")), as.Date("1985-11-05"))
# [1] 1
find.breakpoint(as.Date(c("1985-11-06", "1985-12-01")), as.Date("1985-11-05"))
# [1] NA
