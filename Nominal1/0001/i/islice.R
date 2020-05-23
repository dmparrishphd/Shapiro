islice <- function(i)
        matrix2(c(L, i %|% except.last %|% succ, i))

    Doc$islice <- '
        islice is intended for computing contiguous groups of
        vector elements. Under this interpretation, the argument
        is a vector of upper bounds of said contiguous regions.
        The return is a two-column matrix of values where the
        second column contains the upper bounds and the first
        column contains the lower bounds.
        
        Rows of the return may be applied to the first two
        arguments of seq (`from` and `to`) in order to specify
        each element.
        
        > islice(c(1, 2, 3, 5, 8, 13))

             [,1] [,2]

        [1,]    1    1

        [2,]    2    2

        [3,]    3    3

        [4,]    4    5

        [5,]    6    8

        [6,]    9   13
        '
