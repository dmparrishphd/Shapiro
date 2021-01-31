l.flatten.l <- function(l)
{ # l is a compound list
	list() -> ll
	for (item in l) ll %,% item -> ll
	ll }
