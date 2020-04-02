idiagonal <- function(nsides, ivertex=1L, jdiagonal=1L)
		(ivertex + jdiagonal + 1L) %mod1% nsides
