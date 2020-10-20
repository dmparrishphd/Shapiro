runbind <- function(x, i=0) {
	if (!i) i <- x %|% nrow %/% 2L
	list(x[seq(i),], x[-seq(i),]) }
