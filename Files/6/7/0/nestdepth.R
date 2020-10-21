nestdepth <- function(X) {
    ERR.MSG <- "Argument is not a nest."
    k <- 0L
    repeat {
        if (!is.list(X)) stop(ERR.MSG)
        if (!length(X)) return (k)
        if (length(X) != 2) stop(ERR.MSG)
        k[1] <- 1L + k
        X <- X[[2]] } }
