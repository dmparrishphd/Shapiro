i.index.n <- function(factors, n) {
    n   <- n - 1L # internally, index from zero
    lgf <- length(factors)
    i   <- integer(lgf)
    for (m in rev(1:lgf)) {
        i[m] <- n      %/% factors[m]
        n    <- n - i[m] * factors[m] }
    # adjust back to indexing from one
    1L + i }
