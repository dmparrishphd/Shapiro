fade1 <- function(col="#000000", alpha=.5)
	col %//% (
	clamp(alpha, max_=1 - 2^-52) %|% r.d %|% as.character %|% uppeR)
