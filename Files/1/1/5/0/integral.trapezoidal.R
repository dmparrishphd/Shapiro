integral.trapezoidal <- function(x, y)
		diff(x) * (y[-1] + y[-length(y)]) / 2
