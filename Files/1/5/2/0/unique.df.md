unique.df
=========

Usage
-----

    unique.df(X, ...)
    
| Argument | Description              |
| -------: | :----------------------- |
|      `X` | a `data.frame`           |
|    `...` | (passed to `dupilcated`) |

Value
-----

The unique rows of `X` (as a `data.frame`),
where "unique" is defined by a call to `duplicated`.
