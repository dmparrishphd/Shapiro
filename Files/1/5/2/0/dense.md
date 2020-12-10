dense
=====

Create a dense `vector` from sparse data.

Usage
-----

    dense(idense, isparse, sparse)
    
|   Argument | Description                                  |
| ---------: | :------------------------------------------- |
|   `idense` | unique indices of the `vector` to be created |
|  `isparse` | unique indices of the sparse data            |
|   `sparse` | a `vector`                                   |

Details
-------

The `isparse` and `sparse` arguments are expected to be aligned.

Each element of `isparse` is expected to match exactly one element of `idense`.

The type of return is determined by `typeof(sparse)`

Expect strange behavior if the `sparse` argument is not of  of the basic / atomic `vector` types or `list`.

Examples
--------

    dense(1:9, 6:7, 42)
    # [1] NA NA NA NA NA 42 42 NA NA
    dense(LETTERS, c("I", "J"), rep(42, 2))
    # [1] NA NA NA NA NA NA NA NA 42 42 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA



