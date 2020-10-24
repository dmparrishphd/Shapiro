stepgraph <- function (X=NULL, Y=X, ...)
        linegraph(X=lapply(X, stepsx), Y=lapply(Y, stepsy), ...)
