is.nest <- function(X) repeat {
    if (!is.list(X)     ) return (F)
    if (! length(X)     ) return (T)
    if (  length(X) != 2) return (F)
    X <- X[[2]] }
