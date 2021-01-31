zipNamed <- function(X) {
    NAMES  <- lapply(X, names)
    UNAMES <- sort(unique(unlist(NAMES)))
    List   <- rep(list(vector("list", length(X))), length(UNAMES))
    names(List) <- UNAMES
    for (n in seq_along(UNAMES)) for (k in seq_along(X)) {
        Name <- UNAMES[[n]]
        if (Name %in% NAMES[[k]]) List[[Name]][[k]] <- X[[k]][[Name]] }
    List }
