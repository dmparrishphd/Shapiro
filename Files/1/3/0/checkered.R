checkered <- function(nrow, ncol=nrow) {
	Return <- plaid(nrow, ncol)
	Return[Return] <- F
	Return[Return %|% is.na] <- T
	Return }
