union.interval <- function(x, y)
        if (is.disjoint.interval(x, y)) list(x, y) else list(
            range(c(x, y)))
