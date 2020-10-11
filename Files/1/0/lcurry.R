lcurry <- function(f, X) function(...) do.call(f, c(X, list(...)))
