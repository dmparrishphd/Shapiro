n.index <- function (X, indices) {
	i  <- rep_along(NA_integer_, indices)
	bh <- vapply(indices, is.character, T)
	i[!bh] <- indices[!bh] %|% unlist %|% as.integer
	i[ bh] <- n.index.h(X, indices[bh]) %|% as.integer
	i }
