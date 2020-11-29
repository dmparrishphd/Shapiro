is.interval <- function(X)
        is.numeric(X)  &&
        length(X) == 2 &&
        X[1] <= X[2]
