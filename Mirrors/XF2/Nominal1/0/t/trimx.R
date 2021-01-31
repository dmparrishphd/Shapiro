trimx <- function(x, left=1L, right=1L)
        substr(x, 1L + left, nchar(x) - right)
