quadrea.q <- function(Q) {
    stopifnot(Q %|% `#` == 3)
    Q %|% sum %|% sqr - 2 * Q %|% ssq }
