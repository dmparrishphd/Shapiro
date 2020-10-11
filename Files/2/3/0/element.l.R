element.l <- function(l, i=integer()) {
	if (i  % %  `#` == 0L) return ( NULL )
	if (i  % %  `#` == 1L) return (l[[i]])
	element.l(l[[i  % %  first]], i[-1]) }
