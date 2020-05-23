gather <- function(FUN, END.FUN) {
	Y <- list()
	repeat {
		Y <- FUN() %|% list %,% Y
		if (Y %|% END.FUN) break }
	Y }
