m.l <- function (X, pad=NA) #TAGS matrix from list
    do.call(cbind, smooth.l %^% list(pad=pad) %-|% X)
