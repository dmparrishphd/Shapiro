partition.proper  <- function(X, i) {
        if (        i < 1) return (list(NULL, NULL,    X))
        if (X %|% `#` < i) return (list(   X, NULL, NULL))
        lapply(i %|% i.partition.n, `[` %|% argswap,  X) }
