iunif <- function(n, min=1L, max=2L)
        min + as.integer(
            (1L + max - min) * vapply(
                runif(n),
                base::min %<=% (1 - .Machine$double.eps),
                1))
