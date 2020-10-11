extract.by.name <- function(X, NAMES)
        X[NAMES %in% names(X)]
