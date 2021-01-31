leaves <- function(l) {
	ll <- list()
	while (l  % %  `#`) {
		l  <- leaves.gather(l)
		ll <- c(ll, l[[1]])
		l  <- l.flatten.l(l[[2]]) }
	ll }
