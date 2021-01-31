leaves.gather <- function(l) {
	s <- l  % %  b.leaves # selection
	list(l[s], l[!s]) }
