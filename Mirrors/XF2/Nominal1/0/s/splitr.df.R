splitr.df <- function(x, col.names, ...)
        split(x, lapply(col.names, function(g) x[,g]), ...)
