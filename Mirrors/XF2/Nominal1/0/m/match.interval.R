match.interval <- function(X, Y) `dim<-`(
	vapply(Y, FUN.VALUE=logical(length(X)),
		function(y) y[1] <= X & X <= y[2]),
	c(length(X), length(Y) ) )

