nice.i <- function (LENGTH, i) { #TAGS range check valid index
    storage.mode(i) <- "integer"
    bad <- ! range.check1(LENGTH, i)
    for (k in i %|% seq_along) if (bad[k]) i[k] <- NA
    i }
