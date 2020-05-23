vectorize.step <- function(a) {
	if (a %|% is.vector) return (a)
	if (a %|% ndim == 1L) return (a)
	DIM <- dim(a)
	dim(a) <- DIM[1:2] %|% prod %,% DIM[-2:-1]
	a }
