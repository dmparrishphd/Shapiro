xy.box.mouse <- function() {
	xy1 <- usr.mouse()
	xy2 <- usr.mouse()
	matrix(nrow=2, data=c(
         xy1[1] %,% xy2[1] %|% sort,
         xy1[2] %,% xy2[2] %|% sort) ) }
