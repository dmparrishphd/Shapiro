llse <- function(...) #TAGS ls environment
		lapply(
			environments()[-1],
		 	function(X, ...) list(
				ENVIRONMENT=X, LS=ls(X, ...)),
			...)
