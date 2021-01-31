iranges <- function (X)
        lapply(X %|% ranges, as.integer) %|% cbind_l
