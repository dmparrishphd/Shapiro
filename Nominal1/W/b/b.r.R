b.r <- function(x) vapply2(x, `&` %<=% rPOW2 %O% as.logical)
