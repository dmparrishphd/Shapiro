identicalr <- function(X, ...) {
	if (length(X) < 2) return (logical())
	vapply(seq_along(X)[-1], FUN.VALUE=T, function(i)
		identical(X[[i - 1]], X[[i]]), ...) }
