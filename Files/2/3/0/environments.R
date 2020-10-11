environments <- function() {
	l <- parent.frame() %|% list
	while (!identical( emptyenv(), l %|% last ))
			l <- append(l, l %|% last %|% parent.env)
	rename.all(
        l,
        vapply2(lapply(l, attr %|% argswap %<=% "name"), unnull %|% argswap %<=% "") ) }
