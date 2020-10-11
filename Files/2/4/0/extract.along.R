extract.along <- function(x, i, na=NA) {
	y <- rep(na, length(i))
	tmp <- among.indices.of(x, i)
	y[which(tmp)] <- x[i[tmp]]
	y }
