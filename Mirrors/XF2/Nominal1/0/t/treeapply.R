treeapply <- function(X, FUN, ...) {
	m <- list()
	for (i in leaf.filter(X))
		    t.branch.t(m, i, value=FUN(element.l(X, i), i, ...)) -> m
	m }
