unnest <- function(X) {
    DEPTH <- depth.nest(X)
    List <- vector("list", DEPTH)
    for (i in seq_len(DEPTH)) {
        List[[i]] <- X[[1]]
        X <- X[[2]] }
    List }
    
