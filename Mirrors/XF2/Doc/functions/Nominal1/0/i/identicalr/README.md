identicalr
==========

Usage
-----

identicalr(X, ...)

| Argument | Description                     |
| -------: | :------------------------------ |
|        X | a `vector` or `list`            |
|      ... | arguments passed to `identical` |

Value
-----

A logical vector containing one fewer elements than `X`;
each of the elements tells whether adjacent elements of `X`
are `identical`.

Details
-------

`identicalr` is a kind of generic reflexive xnor.
