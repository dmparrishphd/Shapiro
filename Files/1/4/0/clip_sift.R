clip_sift <- function(x, min_=0, max_=1, include.min=T, include.max=T)
        sift(x, function(x.)
                (if (include.min) `<=` else `<`)(min_, x) &
                (if (include.max) `<=` else `<`)(x, max_))
