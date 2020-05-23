aapply <- function(a, FUN, FUN.VALUE=NULL, ..., USE.NAMES=T) {
    if (FUN.VALUE %|% is.null) FUN.VALUE <- FUN(a[[1]], ...)
    dimension(vapply(a, FUN, FUN.VALUE, ..., USE.NAMES=USE.NAMES), dim(a)) }
