integral.trapezoidal <- function(x, y)
		diff(x) * (y[-1] + y[-length(y)]) / 2

integral.trapezoidal.m <- function(m)
		integral.trapezoidal(m[, 1], m[, 2])
