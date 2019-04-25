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


proportion <- function (x, known, index.known=1)
        known * x / x[index.known]

    Doc$proportion <- '
        proportion returns a scaled version of the primary
        argument (a numeric or complex vector), where the return
        has the "known" value at index.known. Examples:
        
        proportion(0:9, 5, 6) == 0:9

        proportion(1:2, 5, 2) == c(2.5, 5)
        '

i.linterpolate.bases <- function (x, table)
    i.near(table, x) %cbind% ( 1L +
    i.near(table, x, compar=`>=`))

    Doc$i.linterpolate.bases <- '
        i.linterpolate.bases returns the indices of the table
        values which might be used for linear interpolation at the
        values in x. Cf. match.'

linterpolate.weight1 <- function (x, x1)
        x1 %rbind% dimension(x, 1:2) %|% diff %|% abs %|% rev /
        x %|% diff

    Doc$linterpolate.weight1 <- '
        linterpolate.weight1 returns the interpolation weights
        associated with the pair x-values (arg 1) where the
        function is known and the x-value (arg 2) where the
        funciton is to be interpolated.'

linterpolate1 <- function (x, y, x1)
        sum(y * linterpolate.weight1(x, x1))

    Doc$linterpolate1 <- '
        linterpolate1 returns the linearly-interpolated value at
        x1 (arg 3) of the linear function defined by the two
        points represented by the 2x1 columns x and y (args 1
        and 2).'

linterpolate <- function (x, y) {
    # y may contain NA (that's the whole point)
    # y[1] is finite, as is last(y)

    iwhere <- y %|% which.na
    #caterr("iwhere:", iwhere, HEOL)
    xwhere <- x[iwhere]
    xavail <- x[-iwhere]
    yavail <- y[-iwhere]
    ix <- linterpolate.bases(xavail, xwhere) %|% t
    #caterr("ix:", ix, HEOL)
    #caterrhline()
    mx <- matrix(xavail[ix], nrow=2)
    my <- matrix(yavail[ix], nrow=2)
    #cater("mx:")
    #print(mx)
    #caterrhline()
    #cater("my:")
    #print(my)
    #caterrhline()
    #print(y[iwhere])
    vapply_(
        iwhere %|% seq_along,
        function(i) linterpolate1(
            col.m %<=% mx %-|% i,
            col.m %<=% my %-|% i,
            xwhere[i]))
}

.linterpolate <- function (x, y) {
    iwhere <- y %|% which.na
    xwhere <- x[+iwhere]
    xavail <- x[-iwhere]
    ix <- linterpolate.bases(xavail, xwhere) %|% t
    vapply_(
        iwhere %|% seq_along,
        function(i) linterpolate1(
            col.m %<=% matrix(    xavail[ix], nrow=2) %-|% i,
            col.m %<=% matrix(y[-iwhere][ix], nrow=2) %-|% i,
            xwhere[i]))
}

linterpolate <- function (x, y=NULL)
        if (y %|% is.null) {
            .linterpolate(col.m(x, 1), col.m(x, 2)) 
        } else {
            .linterpolate(x, y) }

    Doc$linterpolate <- '
        linterpolate linearly interpolates NA values found in y
        (arg 2 or column 2 of arg 1) based on the non-NA values
        found in x (arg 1 or column 2 of arg 1) and y.

        **** Assumptions ****
        All elements of x are finite.
        first(y) and last(y) are finite.
        '

quadrance0 <- function(xy) rapply(xy ^ 2, sum)

quadrances.m <- function (m, closed=F)
        if (closed) {
            m %rbind% firstr(m) %|% quadrances.m
        } else {
            rapply(m %|% diff, sum.of.squares)
        }

    Doc$quadrances.m <- '
        quadrances.m returns a vector of quadrances between each
        pair of points represented by their Cartesian
        coordinates, found in the rows of the matrix argument.
        The named argument, "closed" determines whether the last
        and first rows are considered a pair of points.'

quadrances.l <- function (X, closed=F)
        quadrances.m(m.l(X, pad=0L) %|% t, closed=closed)

    Doc$quadrances.m <- '
        quadrances.l returns a vector of quadrances between each
        pair of points represented by their Cartesian
        coordinates, found in the elements of the
        list-of-vectors argument.  The named argument, "closed"
        determines whether the last and first points are
        considered a pair of points. The dimensions of the
        points need not be consistent; if they are not, coords
        of the "missing" dimensions will be assumed zero.'

quadrea.quadrances <- function (quadrances)
        sum(quadrances) ^ 2 / 16 - sum(quadrances ^ 2) / 8

    Doc$quadrea.quadrances <- '
        quadrea.quadrances returns the quadrea of the triangle
        represented by the 3-vector of quadrances (arg 1) ****
        ASSUMPTION: **** the vector argument contains exactly
        three values.'

quadrea.l <- list() %,% T %=:% "closed" %v% quadrances.l %O% quadrea.quadrances

    Doc$quadrea.l <- '
        quadrea.l returns the quadrea of the triangle
        represented by the list-of-points argument. ****
        ASSUMPTION: **** the list argument contains exactly
        three points (vectors of coords).'

qaltitude.quadrances <- function (x)
        quadrea.quadrances %-|% x * 4 / x[1]

    Doc$qaltitude.quadrances <- '
        qaltitude.quadrances returns the square-altitude
        (quadrance of altitude) of the triangle whose quadrances
        are given in the 3-vector argument, where the **** FIRST
        ELEMENT IS THE QUADRANCE OF THE BASE ****.'

qaspectratio.quadrances <- function (x) {
        qaltitude.quadrances
     }

'
function quadrance0 : length distance


        The quadrance0 function receives a matrix of
        n-dimensional points (one point per row), and returns a
        vector of quadrances from the origin to each point.

        The quadrance between two points in Cartesian space is
        the sum of the squares of the differences between the
        corresponding coordinates. Quadrance is analogous to the
        square of the length between two points.

        The term "quadrance" is attributed to NJ Wildberger.
'
