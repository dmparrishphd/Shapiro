vapply_ <- function (X, FUN, ...) vapply(
    X, FUN,
    if (X %|% `#`) { lapply(X[1], FUN, ...)                 %|% first
    } else         { lapply(vector(typeof(X), 1), FUN, ...) %|% first },
    ..., USE.NAMES=F) 
