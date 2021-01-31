matches <- function(x, Table, ...)
	Table[,2][match(x, Table[,1], ...)]
