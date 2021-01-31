mode.statistic <- function(x) {
    U <- unique(x)
    LENGTHS <- vapply(U, function(u) sum(x == u), 1L)
    U[LENGTHS == max(LENGTHS)] }
