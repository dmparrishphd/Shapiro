pseudo.log <- function(n, base=10L)
        vapply(n, .pseudo.log %<=% base, 1L)
