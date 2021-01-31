orderr <- function(X, ...)
        X[do.call(order, lapply(list(...), function(x) X[,x])),]
