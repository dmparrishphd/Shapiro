with_empty.names <- function(X) {
    names(X) <- X %|% `#` %|% empty.names
    X }
