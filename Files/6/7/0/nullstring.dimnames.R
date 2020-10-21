nullstring.dimnames <- function(X)
        lapply(X %|% dim, rep %<=% "")

    Doc$nullstring.dimnames <- '
        nullstring.dimnames returns a list that may be used as
        the `value` argument of the `dimnames<-` function; doing
        so produces an object with all null string dimnames.'
