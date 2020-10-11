default <- function (x, default) {
    x[x %|% is.na] <- default;   x }
