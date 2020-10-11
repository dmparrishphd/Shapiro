dichromatic1 <- function(h=0, h2=NULL, n=16, alpha=NULL) {
	if (!is.null(alpha)) warning("behavior of alpha parameter subject to change. not implemented at this time.")
	h2 <- if(h2 %|% is.null) frac(h + .5) else h2
	(if (n %% 2) .dichromatic1.odd else .dichromatic1.even)(h, h2, n, alpha) }
