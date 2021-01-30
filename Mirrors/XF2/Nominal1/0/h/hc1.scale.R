hc1.scale <- function(breaks, h=0, a=1, ...) {
	CENTERS <- capply(breaks %|% fdpairs, mean)
    CENTERSR <- CENTERS %|% range
	CENTERS01 <- rescale(CENTERS, CENTERSR %|% range)
    hc1(c=CENTERS01, a=a,
        h=rescale(x=CENTERS01, to=h %[mod% 1:2) ) }
