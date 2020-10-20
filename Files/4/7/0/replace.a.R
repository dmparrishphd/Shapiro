replace.a <- function(a, list.=lapply(a %|% dim, seq), values=a %|% as.vector)
		do.call %<=% `[<-` %-|% c(
			a %|% list,
			list., 
			recycle_len(values, vapply_ %<=% list. %-|% `#` %|% prod) %|% list)
