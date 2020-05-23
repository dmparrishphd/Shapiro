# Shapiro: A Handsome Helper for R
# Copyright (C) 2019 D. Michael Parrish
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


n.d <- ifloor %O% succ

    Doc$n.d <- '
        n.d is a many-to-one map of the double-s to the integer-s.
        The double-s are binned into the intervals ...,
        [-1, 0), [0, 1), [1, 2), ... . Nonnegative double-s are
        mapped to the positive integers.

        Originally intended to intentify matrix indices from user
        coords of a plot of a plot of the matrix. For example, if a
        2x2 matrix m is plotted with image(x=0:2, y=0:2, m, useRaster=T)
        then n.d maps any user coord in [0,2), [0,2) to the corresponding
        matrix index for the cell containing that user coord.'

cap <- function (x, max.) { #TAGS clamp clip min #SEE_ALSO amp
    i <- na2F.b(x > max.)
    x[i] <- max. %[mod% (i %|% which)
    x }

    Doc$cap <- '
        cap returns a modified copy of arg 1, a numeric vector.
        The return is equal to arg 1, except that the values of
        arg 1 that are greater than the corresponding values of
        max.  (arg 2) are equal to max. Elements of max are
        recycled if necesary to obtain elements corresponding to
        those of arg 1.
        
        > cap(0:9, c(1, Inf))

        [1] 0 1 1 3 1 5 1 7 1 9'

amp <- function (x, min.) #TAGS cap clamp clip
        -cap(-x, -min.)

lift <- amp #DEPRECATED USE amp

    Doc$amp <- '
        amp is somewhat the opposite of cap: the return does
        not contain values less than min. (arg 2).'

clamp <- function (x, min_=0L, max_=0L)
        lift(cap %<=% x %-|% max_, min_)

    Doc$clamp <- '
        clamp returns a vector of the same length as the primary
        argument vector.
        
        Each element of the return is equal to
        the corresponding element of the argument, provided that
        these values are not below the corresponding element of
        min_ and not above the corresponding element of max_.

        Otherwise, the elements of the return corresponding to
        the elements of the argument that are greater than max_
        are equal to max_
        
        and the elements of the return corresponding to the
        elements of the argument that are less than min_ are
        eqal to min_.'

pt.runif <- function (ndim, n, ...)
        vapply(
            ndim %|% seq,
            function(., ...) runif(n, ...),
            n %|% double,
            ...) %|% t

b.near.pt..pp <- function (target, candidates, tolerance=1, quolerance=tolerance ^ 2)
    target %cbind% candidates %|% quadrances.starburst.pt <= quolerance

    Doc$b.near.pt..pp <- '
        b.near.pt..pp returns a logical vector indicating which of
        the candidates (arg 2; points) are near (within
        "length" tolerance, arg 3 or qu[adrance-t]olerance, arg
        4) the target (arg 1; a point). If both tolerance and
        quolerance are specified, tolerance is ignored.
    
    xy <- arrayInd(1:10000, 100 %,% 100) %|% pred / 100
    candidates <- xy %|% t
    target <- c(0, 0)
    plot(xy, col=c(RED, GRN)[b.near.pt(0 %,% 0, xy %|% t, .25, .05) %|% index.b])
    '

csum <- function (m)
        capply(m %|% as.matrix, sum)

    Doc$csum <- '
        csum returns the along-column sums of the matrix
        argument; a vector argument will be treated as a column
        vector.'

quadrances.pp..pp <- square.diffs %O% csum
#specialize("quadrances.pp..pp", n=1) # CREATES `%quadrances.pp..pp%`

    Doc$quadrances.pp..pp <- '
        quadrances.pp..pp returns the between-point quadrances
        of the two polypoint arguments.'

snap.pp..pp <- function (from, to, tolerance=.5, quolerance=tolerance ^ 2) {
    i <- from %|% as.pp %quadrances.pp..pp% as.pp(to) <= quolerance
    Return <- from
    for(k in from %|% colNos) if (i[k]) Return[, k] <- to[, k]
    Return }

    Doc$snap.pp..pp <- '
        snap.pp..pp returns a polypoint from the two polypoint
        arguments. Each point in the return is taken from `to`
        if it is withing a "length" tolerance of `from`.
        Otherwise the point in question is taken from `from`.
        
        If both tolerance and quolerance are specified, only
        quolerance is used.
        
        See also: snap.'

snap <- function (from, to, ...)
        snap.pp..pp(from %|% t, to %|% t, ...) %|% as.vector

    Doc$snap <- '
        snap returns a numeric vector from the two numeric vector
        arguments. Each element of the return is taken from `to`
        if it is within a "length" tolerance of `from`.
        Otherwise the element in question is taken from `from`.
        
        If both tolerance and quolerance are specified, only
        quolerance is used.
        
        See also: snap.pp..pp.'

