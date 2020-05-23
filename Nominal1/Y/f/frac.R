frac <- function(x) x %|% sign * (x %|% abs - x %|% abs %|% floor)
