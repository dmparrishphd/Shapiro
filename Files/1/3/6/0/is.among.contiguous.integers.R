is.among.contiguous.integers <- function(x)
    if (x %|% is_numeric.integer) { inrange(x, INTEGRAL.RANGE)
    } else { rep(F, x %|% `#`) }
