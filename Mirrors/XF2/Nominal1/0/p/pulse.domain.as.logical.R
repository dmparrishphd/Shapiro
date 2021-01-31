pulse.domain.as.logical <- function(i, length=max(i)) {
	y <- logical(length)
	y[i] <- T
	y }
