dms <- function (x) {
    DIR <- x %|% sign %|% as.integer
    x <- x %|% abs
    D <- x %|% floor
    x <- (x - D) * 60
    M <- x %|% floor
    S <- (x - M) * 60
    Si <- S %|% floor
    Sf <- S - Si
    data.frame(D, M, S=Si, R=Sf, IDIR=DIR) }
