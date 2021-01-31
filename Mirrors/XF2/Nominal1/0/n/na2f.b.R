na2F.b <- function (b)
        b %|% is.na %|% `!` & b
