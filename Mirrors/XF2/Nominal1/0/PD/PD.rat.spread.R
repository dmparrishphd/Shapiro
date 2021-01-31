.rat.spread <- function (m) c(
    m %|% det2 %|% sqr,
    m[,1] %|% ssq * m[,2] %|% ssq )
