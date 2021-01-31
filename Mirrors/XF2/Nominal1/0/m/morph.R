morph <- function(X, FUN, test=function(y)T, ...)
		lapply(X, function(x, ...) if (test(x)) FUN(x, ...) else x, ...)
