fn.explode <- function(FUN)
		function(...) FUN(unlist(list(...)))
