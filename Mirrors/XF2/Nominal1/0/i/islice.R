islice <- function(i)
        matrix2(c(L, i %|% except.last %|% succ, i))
