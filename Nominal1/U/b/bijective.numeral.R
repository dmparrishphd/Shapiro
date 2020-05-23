bijective.numeral <- function(n, base=26L) {
    if (n  < 0) return (list())
    if (n == 0) return (0 %|% integer %|% list)
    intermediate <- pseudo.log(n, base) %|% integer
    m <- 0L
    while (n) {
        m <- 1L + m
        intermediate[[m]] <- 1:base %[mod% n
        n <- n %|% pred %/% base }
    intermediate[1:m] %|% list }
