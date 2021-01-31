.solveline <- function(line, x=NULL, y=NULL)
		if (is.null(y)) {
			solveline.x(line, x)
		} else {
			solveline.y(line, y) }
