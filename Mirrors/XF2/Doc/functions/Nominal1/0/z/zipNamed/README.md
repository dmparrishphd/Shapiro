zipNamed
========

Usage
-----

zipNamed(X)

Arguments
---------

| Argument | Description          |
| -------: | :------------------- |
|        X | A `list` of `list`s. |

Value
-----

A list containing the _named_ items of the elements of `X`.

The same-named items of the elements of `X` are grouped by name.

Examples
--------

zipNamed(list(
    list(earth="brown", sky="blue"),
    list(earth="solid", sky="gas")))


