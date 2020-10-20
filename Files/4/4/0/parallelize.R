parallelize <- function(...)
        do.call(cbind, lapply(list(...), as.vector))
