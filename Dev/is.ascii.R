is.ascii <- function (.character) {
    stop("UNTESTED")
    vapply(.character, FUN.VALUE=T, function(x)
        if (x %|% is.na %|% not && x %|% nchar) {
            all(x %|% utf8ToInt) < 128
        } else {
            F } ) }
