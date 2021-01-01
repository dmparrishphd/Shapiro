with_dimnames.from.within <- function(
        X,
        isample=rep(1L, X %|% ndim),
        direction=1L) {
    NAMES <- core(X, isample, direction) %|% as.character
    Dimnames <- if (X %|% dimnames %|% is.null)
            nullstring.dimnames(X) else dimnames(X)
    Dimnames[[direction]] <- NAMES
    dimnames(X) <- Dimnames
    X }

    Doc$with_dimnames.from.within <- '
        with_dimnames_from_within returns a modified copy of arg
        1 (an array or data.frame), where the dimnames along the
        direction (arg 3) given have been taken from one of the
        "cores" through the sample cell specified by arg 2.'
