pulse.domains <- function(X) {
	if (length(X) < 2) return (X)
	b <- sort(unique(unlist(X)))
	Y <- as.data.frame(optional=T,
			rbind(b[-length(b)], b[-1]))
	M <- vapply(Y, mean, 1)
	i <- vapply(seq_along(M), FUN.VALUE=T, function(k)
		any(vapply(X, FUN.VALUE=T, function(x)
			x[1] <= M[k] && M[k] <= x[2])))
	Y[i][,order(Y[i][1,]), drop=F] }
