.place.values <- function(base, n, bits=64) {
        lsd. <- lsd %<=2% base
        m <- O
        y <- integer(bits)
        while (0 < n) {
            m    <- m + L
            y[m] <- n %|% lsd.
            n    <- (n - y[m]) %/% base }
        y[y %|% which1rev %|% seq] }
