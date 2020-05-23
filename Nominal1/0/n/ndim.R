ndim <- function (a)
        max(1L, a %|% dim %|% `#`)
