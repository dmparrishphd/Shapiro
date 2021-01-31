vapply1to1 <- function (X, FUNs, FUN.VALUE, ...)
        vapply(
            X %|% seq_along,
            function(n) `%[[mod%`(FUNs, n)(X %[[% n, ...),
            FUN.VALUE)
