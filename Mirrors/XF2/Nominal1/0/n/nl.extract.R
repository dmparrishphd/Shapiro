nl.extract <- function(X, indices=list()) {
	r <- l.extract(X, indices)
    if (X %|% names %|% is.null %|% `!`) names(r) <- names(X)[n.index(X, indices)]
	r }
