`%v%` <- function (LIST, FUN) do.call(curry, c(FUN, LIST))
