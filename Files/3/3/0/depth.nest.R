depth.nest <- function(X) {
    k <- 0L
    repeat {
        stopifnot(is.list(X))
        if (!length(X)) return (k)
        stopifnot(length(X) == 2)
        X <- X[[2]]
        k <- k + 1L } }
