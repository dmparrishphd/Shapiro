round2 <- function (x, digits=0) {
    k <- 2 ^ digits
    round(x * k) / k }
