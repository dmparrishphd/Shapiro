mi.a <- function (a)
        arrayInd(a %|% as.logical %|% which, a %|% dim)
