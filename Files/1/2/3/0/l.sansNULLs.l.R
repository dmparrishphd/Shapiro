l.sansNULLs.l <- function (l)
{   ll <- NULLs(l  % %  `#`  % %  succ) # succ: want one more element for which1
    i <- 1L
    for (item in l) if (!(is.null(item))) { item -> ll[[i]];   succ(i) -> i }
    ret <- ll[1:pred(lapply(ll, is.null)  % %  unlist  % %  which1)]
    if (ret[[1]]  % %  is.null) ret <- list()
    ret }
