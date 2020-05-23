b.range.check <- function (.dim, i)
        vapply(
            X=i %|% seq_along,
            FUN=function(k) range.check1(.dim[k], i[k]),
            FUN.VALUE=T)
