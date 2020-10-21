mi.core <- function(.dim, isample, direction) { #TAGS indices along dimension
    mi <- matrix(rep(isample, .dim[direction]), .dim %|% `#`) %|% t
    mi[,direction] <- .dim[direction] %|% seq
    mi }

    Doc$mi.core <- '
        mi.core returns an index matrix that specifies a "core"
        in a given direction (arg 3) through the element
        specified by isample (arg 2) of an array or data.frame
        of the given dimensions (arg 1).

        > mi.core(dim(iris), c(5, 1), 2)

        #      [,1] [,2]

        # [1,]    5    1

        # [2,]    5    2

        # [3,]    5    3

        # [4,]    5    4

        # [5,]    5    5
        '
