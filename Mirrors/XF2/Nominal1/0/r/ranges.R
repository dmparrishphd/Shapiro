ranges <- function(X)
    enlist(
        if (X %|% is.vector) {
            if (X %|% is.list) {
                X %|% .ranges.l
            } else {
                X %|% range.finite
            }
        } else if (X %|% is.matrix) {
            X %|% .ranges.m
        } else if (X %|% is.data.frame) {
            X %|% .ranges.m
        } else {
            list() } )
