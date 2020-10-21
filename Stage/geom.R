# Shapiro: A Handsome Helper for R
# Copyright (C) 2019 D. Michael Parrish
# COPYRIGHT NOTICE CONTINUES AT ./COPYRIGHT2.txt
# 
# BRIEF TABLE OF CONTENTS
#
# ./COPYRIGHT1.R                     Copyright Notice (PART 1/2)
# ./COPYRIGHT2.R                     Copyright Notice (PART 2/2)
# ./LICESE.txt                                 License (Primary)
# ./LICENSE-Stack_Overflow.htm                 License for curry
# *.R                                   (Body / Primary Content)

'""
pp # POLYPOINT #SEE ALSO: doc(pp)
""'

    Doc$pp <- '
        pp is a prefix, infix, or suffix used in some names to
        indicate a polypoint. Herein, a polypoint is a matrix of
        points, one point per column, one row per dimension. If
        the polypoint is in 2D space, it-s transpose is compatible
        with the R graphics::points function.'

'""
lpp # LIST-POLYPOINT #SEE ALSO: doc(lpp)
""'

    Doc$lpp <- '
        lpp is a prefix, infix, or suffix used in some names to
        indicate a list-polypoint. Herein, a list-polypoint is a list of
        points (all of the same dimension), one point per item.

        See also pp.lpp.'

is.lpp <- function(x)
        none(vapply2(x, mode) != "numeric") &
        none(vapply2(x, class) %notin% words("integer numeric")) &
        vapply2(x, `#`) %|% diff %|% none

.pp.lpp <- function(l)
        if (l %|% is.empty) m.empty() else
                matrix(l %|% unlist, l[[1]] %|% `#`)

pp.lpp <- function(x)
        list(nop, .pp.lpp)[[x %|% is.lpp %|% index.b]](x)

    Doc$pp.lpp <- '
        pp.lpp returns a pp (polypoint) object from the given
        lpp (list-polypoint) object.'

is.pp <- function (X)
        X %|% is.matrix & X %|% mode == "numeric"

as.pp <- function (X)
        if (X %|% is.pp) { X
        } else if (is.vector(X, "numeric")) { X %|% matrix
        } else {}

'""
[.pp #FUTURE? #SEE ALSO: extract.pp #TAGS extract [
""'

extract.pp <- function(.pp, i)
        .pp[, i, drop=F]

    Doc$extract.pp <- '
        extract.pp returns a pp (polypoint) object formed by
        extracting the specified elements i (arg 2) from the
        given pp object (arg 1).
        
        Arg 2 (i) may be any index that is allowed by `[` for
        selecting columns of a matrix.'

is.points <- is.pp #DEPRECATED USE is.pp
as.points <- as.pp #DEPRECATED USE as.pp

    Doc$is.pp <- '
        is.pp returns a logical value that tells whether the
        argument may be interpreted as a collection of points: a
        numeric matrix, one row per dimension, on column per
        point.'

    Doc$is.points <- '
        is.points - DEPRECATED - is an alias for is.pp.
    
        is.points **** SHOULD NOT BE CONFUSED **** with graphics::points.'

    Doc$as.pp <- '
        as.pp returns the argument as a "proper" points
        object, provided that the argument may be interpreted as
        points.' 

    Doc$as.points <- '
        as.points - DEPRECATED - is an alias for as.pp.'

`#.pp` <- ncol

    Doc$`#.pp` <- '
        `#.pp` returns the number of points in the polypoint
        argument.'

igrid.regular <- function (.dim)
        arrayInd(.dim %|% prod %|% seq, .dim)

    Doc$igrid.regular <- '
        igrid.regular returns a matrix index of the grid-centers
        of a reguar grid (topologically Cartesian) having the
        dimensions given by the argument.'

grid_regular.aligned <- function (X) {
    i <- igrid.regular(vapply(X, `#`, 1L))
    vapply(
        X %|% seq_along,
        function(k) X[[k]][i[,k]],
        vector(mode=X[[1]] %|% typeof, length=i %|% nrow)) }

    Doc$grid_regular.aligned <- '
        grid_regular.aligned returns a matrix of n-dimensional
        grid-centers of a reguar grid (Cartesian). The
        coordinates of the points are specified in a
        per-direction basis by the list argument, which has one
        element per direction.

        > grid_regular.aligned(list(10 * 1:2, 100 * 1:3))

             [,1] [,2]
        [1,]   10  100
        [2,]   20  100
        [3,]   10  200
        [4,]   20  200
        [5,]   10  300
        [6,]   20  300
    '

centroid <- function (triangle)
        capply(triangle, mean)

dms <- function (x) {
    DIR <- x %|% sign %|% as.integer
    x <- x %|% abs
    D <- x %|% floor
    x <- (x - D) * 60
    M <- x %|% floor
    S <- (x - M) * 60
    Si <- S %|% floor
    Sf <- S - Si
    data.frame(D, M, S=Si, R=Sf, IDIR=DIR) }

    Doc$dms <- '
        dms returns a data frame of DEGREES MINUTES SECONDS
        DIRECTION, with one row per element of the numeric
        vector argument (which is assumed to be be in units of
        degrees).'

dot.prod <- `*` %O% sum #TAGS dot product

reimann <- dot.prod #TAGS Reimann sum

    Doc$reimann <- '
        reiman is the same as dot.prod.

        If one of the arguments is the height of the piecewise
        constant values approximating a function and the other
        is the width of those piecewise constant segments, the
        return is the Reimann sum.'

    Doc$dot.prod <- '
        dot.prod returns the dot product of the two vector
        arguments.

        > dot.prod(1:3, 1:3)

        [1] 14
        '

fn.rescale <- function (from, to)
        #TODO: REWRITE IN TERMS OF WILDBERGERS LINES:
        #       cbind(from, to) ...
        if (to %|% `#` < 2) {
            return (function(x) to)
        } else {
            function(x) to[1] + (x - from[1]) * diff(to) / diff(from) }

    Doc$fn.rescale <- '
        fn.rescale returns a function that may be used to
        transform scalars from one LINEAR scale to another.

        > C2F <- fn.rescale(x=c(0, 100), y=c(32, 212))

        > C2F(37)

        [1] 98.6

        > C2F(-40)

        [1] -40'

fn.rescale.i2d <- fn.rescale

    Doc$fn.rescale.i2d <- '
        fn.rescale.i2d is an integer-to-double version of
        fn.rescale; it is an alias that may be used to allow
        code to be more informative as to the intent.'

fn.rescale.d2i <- function (from, to) {
    DX <- diff(from) / diff(to)
    function(x) to[1] + (DX/2 + x - from[1]) %/% DX }

    Doc$fn.rescale.d2i <- '
        fn.rescale.d2i is a GRID-CENTERED double-to-integer
        version of fn.rescale. If the from and to arguments are
        equal, integer-valued vectors, the return is a function
        that rounds to the nearest integer, where edge cases
        round UP.  See also dwg fn-rescale-d2i.pdf'

fn.numerator <- function(n=256)
        fn.rescale.d2i(from=c(.5, n - .5)/n, to=ol*(n-1))

    Doc$fn.numerator <- '
        fn.numerator returns a function that maps the interval
        [0, 1) to seq(n) - 1. The return of the return is a
         number that may be interpreted as the numerator of the
         corresponding fraction, where the denominator is the
         argument of fn.numerator.'

numerator <- function(d, denominator=256)
        denominator %|% fn.numerator %-|% d
    
    Doc$numerator <- '
        numerator returns the numerator(s) of the proper
        fraction vector argument (a double vector whose elements
        are in [0, 1)), where the denominator is given by the
        argument of that name. Internally, each of the
        denominator subintervals of [0, 1) are of equal width
        (machine precisoin). See also Doc/numerator.pdf'

r.d <- numerator %O% as.raw

    Doc$r.d <- '
        r.d returns a raw vector that represents the double
        vector of values in [0, 1). There is **** NO CHECKING
        **** that the argument contains values outside this
        range.

        > (c(0, 1,255)/256) %|% r.d

        [1] 00 01 ff'

irescale.d <- function (x, from=0:1, to=0:1) {
fn.rescale.d2i(from=from, to=to)
     }

rescale <- function (x, from=0:1, to=0:1)
        vapply(x, fn.rescale(from, to), 1)

    Doc$rescale <- '
        rescale maps the elmenets of the vector (arg 1) from
        (arg 2) the range given to (arg 3) the range given.'

rescale.self <- function (x, to=0:1)
        rescale(x, from=x %|% range.finite, to=to)

    Doc$rescale.self <- '
        rescale.self maps the elmenets of the vector (arg 1) to
        the range given ("to", arg 2). **** DESIGNED **** for
        cases where there are at least two, distinct, finite
        values in arg 1.'

degrees <- function(x) x * pi/180

    Doc$degrees <- '
        degrees returns the angle measure(s) in radians that
        corresponds to the angle measure(s) in degrees found in the
        argument. Intended usage, e.g.: 30 %|% degrees.'


