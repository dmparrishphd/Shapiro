interlace1 <- function (list.of.parallel.vectors)
        do.call(rbind, list.of.parallel.vectors) %|% as.vector
