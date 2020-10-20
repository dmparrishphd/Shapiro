rescale <- function (x, from=0:1, to=0:1)
        vapply(x, fn.rescale(from, to), 1)
