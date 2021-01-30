alg.rel.diff

DESCRIPTION

        Algebraic relative difference

USAGE

        alg.rel.diff(x, y)

ARGUMENTS

    `x`, `y`

        Numeric vectors of equal length.

VALUE

        A numeric vector with one element per element of `x`.
        Each element of the return stores the algebraic
        relative difference corresponding with elements of `x`

DETAILS

        See the README accompanying `alg.rel.diff2` for
        additional details.

SEE ALSO

        `alg.rel.diff2`

EXAMPLES

        alg.rel.diff(c(0,0,0,1,1,Inf), c(0,1,Inf,1,Inf,Inf))

        # [1]   0   1   1   0   1 NaN
