mi.core <- function(.dim, isample, direction) { #TAGS indices along dimension
    mi <- matrix(rep(isample, .dim[direction]), .dim %|% `#`) %|% t
    mi[,direction] <- .dim[direction] %|% seq
    mi }
