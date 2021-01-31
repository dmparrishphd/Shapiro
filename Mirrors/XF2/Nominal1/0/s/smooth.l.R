smooth.l <- function (X, pad=NA) #TAGS ragged array
        lapply(
            X,
            extend.to.length %^%
                    list(
                        length.out=vapply_(X, `#`) %,% 0L %|% max,
                        values=pad))
