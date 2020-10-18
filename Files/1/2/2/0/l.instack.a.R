l.unstack.a <- function (a)
    if (a %|% dim %|% `#` < 2) { list(a)
    } else {
        LENGTH <- a %|% slice.big.length
        lapply(
            a %|% dim %|% last %|% seq %|% pred,
            function(n) dimension(
                a[LENGTH %|% seq + n * LENGTH],
                a %|% slice.big.dim) ) }
