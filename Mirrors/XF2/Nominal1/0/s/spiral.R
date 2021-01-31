spiral <- function(n=100, s=1/2, w=GOLDEN.RATIO) {
	t <- n %|% seq %|% pred %|% sqrt * s
	a <- 2 * pi * w * t
	t * cbind(a %|% cos, a %|% sin) }
