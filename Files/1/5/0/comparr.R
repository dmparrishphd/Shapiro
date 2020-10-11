comparr <- function (
        extract, store=catn, compar=`-`, nmax=Inf,
        compar.sentinel=identical, verbose=F) {
	if (nmax < 1L) return (invisible())
	SENTINEL <- extract()
	i <- 1L
	Prev <- i %|% extract
	if (compar.sentinel(Prev, SENTINEL))
			return (invisible())
	repeat {
		if (nmax < i) return (invisible())
		i <- i + 1L
        if (verbose) {
            catn("make.comparison (about to extract Next): i:", i)
        }
		Next <- i %|% extract
        if (verbose) {
            catn("make.comparison: i:", i)
            catn("make.comparison: kind(Next):---")
            Next %|% kind %|% print
            catn("make.comparison: kind(SENTINEL):---")
            SENTINEL %|% kind %|% print
            catn("make.comparison: length(SENTINEL):---")
            SENTINEL %|% length %|% print
        }
		if (compar.sentinel(Next, SENTINEL))
				return (invisible())
		compar(Next, Prev) %|% store
		Prev <- Next }
	invisible() }
