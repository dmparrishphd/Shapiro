recycle <- function(x, FUN) {
    result = x
    while (!FUN(result)) result <- c(result, x);   result }
