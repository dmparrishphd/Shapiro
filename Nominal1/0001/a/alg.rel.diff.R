alg.rel.diff <- function(x, y) {
    m <- alg.rel.diff2(x, y)
    Return <- m[,1] / m[,2]
    Return[!m[,1]] <- 0 # NUMERATOR ZERO
    Return[is.infinite(m[,1])] <- 1
    # CASE WEHRE NUMERATOR IS NaN TAKES CARE OF ITSELF
    Return }
