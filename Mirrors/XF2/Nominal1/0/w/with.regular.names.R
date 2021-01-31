with_regular.names <- function(X) {
    names(X) <- X %|% `#` %|% regular.names
    X }
