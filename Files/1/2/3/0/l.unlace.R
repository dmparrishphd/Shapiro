l.unlace <- function(l, ncol=2) lapply(
		ncol %|% seq,
		function(offset) as.list(l)[
            seq(l %|% `#` %/% ncol) %|% pred * ncol + offset])
