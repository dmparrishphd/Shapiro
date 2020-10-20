vapply2 <- function (X, FUN) vapply(
    X, FUN, USE.NAMES=F,
    if (X %|% `#`) { lapply(X[1], FUN)                 %|% first
    } else         { lapply(vector(typeof(X), 1), FUN) %|% first })
