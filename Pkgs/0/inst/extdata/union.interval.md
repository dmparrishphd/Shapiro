union.interval
==============

Usage
-----

    union.interval(x, y)

| Argument | Description |
| -------: | :---------- |
|        x | An interval |
|        y | An interval |

Value
-----

A `list` containing the union of the two interval arguments.

Examples
--------

    union.interval(0:1, 1:2)
    # [[1]]
    # [1] 0 2
    union.interval(0:1, c(2, 2))
    # [[1]]
    # [1] 0 1
    #
    # [[2]]
    # [1] 2 2
    union.interval(0:1, c(.3, .7))
    # [[1]]
    # [1] 0 1
