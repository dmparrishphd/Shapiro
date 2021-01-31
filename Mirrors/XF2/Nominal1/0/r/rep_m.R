rep_m <- function (m, times=1) matrix(
	rep(m %|% t, times), ncol=ncol(m), byrow=T)
