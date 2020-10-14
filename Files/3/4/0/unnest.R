unnest <- function(X) {
    DEPTH <- depth.nest(X)
    List <- rep(list(NULL), DEPTH)
    k <- DEPTH
    while (k) {
        List[[DEPTH - k + 1]] <- X[[1]]
        X <- X[[2]]
        k <- k - 1L }
    List }
