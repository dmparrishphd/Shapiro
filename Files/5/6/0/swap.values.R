swap.values <- function(X, a, b) {
	i <- if (a %|% is.na) X %|% is.na else X == a
	j <- if (b %|% is.na) X %|% is.na else X == b
	X[i] <- b
	X[j] <- a
	X }
