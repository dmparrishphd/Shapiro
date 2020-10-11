cointoss <- function (n=1L) !floor(runif(n, max=2 - .Machine$double.eps))
