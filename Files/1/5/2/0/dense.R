dense <- function(idense, isparse, sparse) {
	Dense <- rep(replace(vector(typeof(sparse), 1), 1, NA), length(idense))
	Dense[match(isparse, idense)] <- sparse
	Dense }
