i.partition.n <- function (n) { #TAGS index indices
        if (n %|% is.numeric %|% `!`) return (3 %|% NULLs)
        if (n  < 1) return (list(integer(), integer(), NULL))
        if (n == 1) return (list(integer(),        1L,  -1L))
         list(
            n %|% pred %|% seq,
            n,
            n %|% sans) }
