irange <- function (x) list(
    FINITE=x %|% which.finite,
    .INF=x %|% which..Inf,
    INF=x %|% which.Inf,
    NAN=x %|% which.NaN,
    NA.=x %|% which.na)
