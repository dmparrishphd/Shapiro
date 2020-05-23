as_integer.b <- function (b) (b %|% `#` %|% ipow2 * b) %|% sum
