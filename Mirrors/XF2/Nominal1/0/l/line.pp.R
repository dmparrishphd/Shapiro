line.pp <- function(pp)
	c(
		det2(pp),
		-pp %|% secondr %|% diff,
		+pp %|% firstr  %|% diff)
