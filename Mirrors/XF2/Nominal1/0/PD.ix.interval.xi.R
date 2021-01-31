.ix.interval.xi <- function (shift, i)
    if (i %|% is.matrix || i %|% is.data.frame) {
        i %m+v% shift
    } else if (i %|% is.vector) {
        i[1:2] %|% as.matrix %|% t %|% x.interval.i }
