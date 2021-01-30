alg.rel.diff2 <- function(x, y) {
	xx <- x * x
	yy <- y * y
    ss <- xx + yy
    ds <- xx - yy
    cbind(ds * ds, ss * ss) }
