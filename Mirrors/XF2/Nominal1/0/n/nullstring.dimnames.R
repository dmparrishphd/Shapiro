nullstring.dimnames <- function(X)
        lapply(X %|% dim, rep %<=% "")
