range. <- function (x) {
    BRANGE <- x %|% nrange
    NAMES  <- BRANGE %|% names
    BRANGE <- rename.all(!!BRANGE, NAMES)
    rename.all(
        c(  
            { if (BRANGE[["FINITE"]]) {
                x %|% finites %|% range %|% as.list
            } else {
                x[0] %|% list %|% rep2 } },
            BRANGE[-1] %|% as.list),
        "MIN MAX" %|% words %,% NAMES[-1]) }
