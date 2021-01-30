allequal <- function (x)
        if (length(x) < 2) T else all(x[1] == x[-1])
