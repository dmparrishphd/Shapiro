crosstabulate <- function(tables) {
	INDEX <- lapply(tables, first) %|% unlist %|% uniques
	CROSS <- data.frame(stringsAsFactors=F, INDEX)
	for (k in tables %|% seq_along)
			CROSS[match(tables[[k]][[1]], INDEX), 1 + k] <- tables[[k]][[2]]
	CROSS }
