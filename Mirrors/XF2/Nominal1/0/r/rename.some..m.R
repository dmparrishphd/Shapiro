rename.some..m <- function(X, mnames) {
	names(X)[match(mnames[,1], names(X))] <- mnames[,2]
	X }
