.list.frame.dim <- function(.dim)
	rep(.dim[1] %|% nulls %|% list, .dim[2])
