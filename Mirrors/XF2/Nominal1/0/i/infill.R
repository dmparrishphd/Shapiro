infill <- function(X, replacement=NA) {
    for (k in X %|% seq_along) if (X[[k]] %|% is.null) X[[k]] <- replacement
    X }
