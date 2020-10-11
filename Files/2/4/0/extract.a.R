extract.a <- function(a, list.=lapply(a %|% dim, seq), drop=T)
		do.call %<=% `[` %-|% c(
			a %|% list,
			list.,
            list(drop=drop)) 
