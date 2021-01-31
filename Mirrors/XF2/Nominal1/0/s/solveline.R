solveline <- function(line, x=NULL, y=NULL) {
		solution <- .solveline(line, x=x, y=y)
		replace(solution, solution %|% is.finite %|% `!`, NA) }
