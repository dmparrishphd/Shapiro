pretty_list <- function(.list, FUN=list(.pretty_list.FUN.default))
		vapply2(.list %|% seq_along, function(k)
				.list[[k]] %|% FUN[[ k %mod1% length(FUN) ]] )
