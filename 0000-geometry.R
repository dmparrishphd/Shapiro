# Shapiro: A Handsome Helper for R
# Copyright (C) 2018 D. Michael Parrish
# 
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public
# License along with this program.  If not, see
# <https://www.gnu.org/licenses/>.
#
# END OF COPYRIGHT NOTICE
#
#

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

degrees <- function(x) x * pi/180

    Doc$degrees <- '
        degrees returns the angle measure(s) in radians that
        corresponds to the angle measure(s) in degrees found in the
        argument. Intended usage, e.g.: 30 %|% degrees.'


