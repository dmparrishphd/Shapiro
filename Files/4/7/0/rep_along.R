rep_along <- function (x=F, along.with=1:2)
        rep_len(x, along.with %|% `#`)
