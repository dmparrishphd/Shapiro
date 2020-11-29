intersect.interval <- function(x, y)
        if (is.disjoint.interval(x, y)) list() else list(c(
            max(x[1], y[1]), min(x[2], y[2])))
