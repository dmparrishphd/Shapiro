binaryToVariadic <- function(FUN) function(...) Reduce(
    f=FUN, init=..1, x=c(list(..2), list(...)[-2:-1]))
