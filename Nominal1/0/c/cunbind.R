cunbind <- function(x, i=0) {
	if (!i) i <- x %|% ncol %/% 2L
	list(x[,seq(i)], x[,-seq(i)]) }
