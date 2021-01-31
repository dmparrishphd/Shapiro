t.branch.t <- function (t, i=integer(), allowMetamporphosis=F, value=NULL) {
	if (i  % %  `#` == 0L) return (t)
    i  % %  first -> j
	t <- l.extend.l(t, j)
    if (i  % %  `#` == 1L)
            if (value  % %  is.null  % %  `!`) t[[j]] <- value
	if (i  % %  `#` >  1L) {
        t[[j]] -> tj
		if (
				(tj  % %  is.leaf) &&
				(tj  % %  is.null  % %  `!`) &&
				!allowMetamporphosis) {
			warning("Contradiction: Metamporphosis both requested and disallowed.")
			return (list()) }
        #
        # don't want to destroy structure that's already there
		if (tj  % %  is.leaf) t[[j]] <- list() 
		t[[j]] <- t.branch.t(tj, i[-1], allowMetamporphosis, value) }
	t }
