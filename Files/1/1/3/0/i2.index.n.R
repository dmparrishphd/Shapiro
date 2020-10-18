i2.index.n <- function(m, n) {
    nr <- nrow(m);   n1 <- n - 1L;   1L + c(n1 %% nr, n1 %/% nr) }
