unnest <- function(X) {
	DEPTH <- X %|% nestdepth # EXPECTED TO stop IF X IS NOT A nest
	if (!DEPTH) return (X)
	Y <- DEPTH %|% nulls
	for (k in DEPTH %|% seq) {
		if (X %|% is.empty) return (Y)
		Y[[k]] <- X %|% first
		X <- X %|% second }
	Y }
