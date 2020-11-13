argswap <- function (FUN) function(x, y, ...) do.call(FUN, c(list(y, x), ...))
