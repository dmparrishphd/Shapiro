l.zip.l <- function (l)
        if (l  %|%  length)
        lapply(Nos.(l[[1]]), l.unparallel, l) else list()
