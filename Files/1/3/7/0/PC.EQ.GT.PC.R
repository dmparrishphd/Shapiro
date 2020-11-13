`%=>%` <- function (X, FUN) do.call(curry, c(FUN, list(X)))
