reflexive <- function (FUN, x) FUN(x %|% rest, x %|% except.last)
