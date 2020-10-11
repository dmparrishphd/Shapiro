dub <- function(X, dfnames) {
	if (X %|% names %|% is.null)
		names(X) <- rep_along(HNULL, X)
	names(X)[dfnames[[1]]] <- dfnames[[2]]
	X }
