partition <- function(what, sep)
        partition.proper(what, match(sep[[1]], what, nomatch=0L))
