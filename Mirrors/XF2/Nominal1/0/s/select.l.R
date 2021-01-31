select.l <- function (X, FUN, ...)
        lapply(X, FUN, ...) %|% unlist %|% which
