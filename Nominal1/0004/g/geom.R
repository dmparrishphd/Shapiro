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


centroid <- function (triangle)
        rapply.(triangle, mean)

    Doc$centroid <- '
        centroid returns the centroid of the triangle argument,
        a 3-column matrix (one for each point of the triangle),
        one row per dimension, an order **** DIFFERENT **** from
        that assumed by plot, points, etc.'

area2 <- function (triangle)
        triangle %|% justify.pt %rbind% 1L %|% det3
                # justify.pt HELPS TO AVOID INTEGER OVERFLOW IN INTERMEDIATE STEPS

area <- .area2 %O% `/2`

    Doc$area <- '
        area returns the signed area of the triangle whose 2-D
        points are represented in the 2x3 matrix argument.
        Dimension 1 is in row 1 and dimension 2 is in row 2.
        Points are considered to be ordered (clockwise vs.
        counterclockwise) [Ref: DP].'

    Doc$area2 <- '
        area2 is the same as area, except that it returns twice
        the area.'

'
i.triangles.starburst <- function (n) #OLD
        n %|% stepsi %,% n %,% 2L %|% rest %|% rest %|% matrix2r %rbind% 1L
'
i.triangles.starburst <- function (n)
        n %|% as.integer %|% stepsi %|% inner %|% inner %,% as.integer(n) %,% 2L %|% matrix2r %rbind% 1L

i.triangles.seashell <- i.triangles.starburst %O% cexcept.last %O% rotater %O% rotater

    Doc$i.triangles.starburst <- '
        i.triangles.starburst returns a matrix, one column per
        triangle of a starburst of the order indicated by the
        argument. The elements of each column indicate the
        points of the triangles of the starburst. The first two
        rows contain the extreme-point indices, while the last
        row contains the index of the central point (1L).'

    Doc$i.triangles.seashell <- '
        i.triangles.seashell returns a matrix, one column per
        triangle of a seashell of the order indicated by the
        argument. The elements of each column indicate the
        points of the triangles of the seashell. The first
        row contains 1L, the common point the extreme-point indices, while the last
        row contains the index of the central point (1L).'

areas.starburst2 <- function (points)
        if (points %|% ncol < 3) vector(mode=points %|% typeof) else
        vapply(
            points %|% ncol %|% pred %|% seq,
            function(j, i) points[, i[,j]] %|% area2,
            1 %|% double,
            points %|% ncol %|% i.triangles.starburst)

areas.starburst <- areas.starburst2 %O% `/2`

    Doc$areas.starburst <- '
        areas.starburst returns the signed areas of the
        triangles of a starburst.'

    Doc$areas.starburst2 <- '
        areas.starburst2 returns the TWICE the signed areas of
        the triangles of a starburst.'

area.coords <- function (polygon, point)
        point %cbind% polygon %|% areas.starburst2

    Doc$area.coords <- '
        area.coords returns pseudo--area-coordinates of the point
        (arg 2) with respect to the polygon (arg 1). The values
        are expected to be interpreted as relative to one
        another or to their sum. Each value is equal to TWICE
        the signed area of the corresponding triangle.'

sign_coords <- function (polygon, points) #TAGS lines
        sign_eval.lines(m.lines.pg(polygon), points)

    Doc$sign_coords <- '
        sign_coords is similar to area.coords, except that more
        than one point may be given, and the result is a matrix
        with one column per point and one row per polyogn-side.'

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
        (x - x1) %|% abs %|% rev / x %|% diff

    Doc$linterpolate.weight1 <- '
        linterpolate.weight1 returns the interpolation weights
        associated with the pair x-values (arg 1) where the
        function is known and the x-value (arg 2) where the
        funciton is to be interpolated.
        
        **** INTENDED **** for cases where diff(x) > 0.'

linterpolate1 <- function (x, y, x1)
        sum(y * linterpolate.weight1(x, x1))

    Doc$linterpolate1 <- '
        linterpolate1 returns the linearly-interpolated value at
        x1 (arg 3) of the linear function defined by the two
        points represented by the 2x1 columns x and y (args 1
        and 2).'

.linterpolate <- function (x, y) {
    # y may contain NA (that's the whole point)
    # y[1] is finite, as is last(y)
    iwhere <- y %|% which.na
    xwhere <- x[+iwhere]
    xavail <- x[-iwhere]
    ix <- i.linterpolate.bases(xavail, xwhere) %|% t
    vapply_(
        iwhere %|% seq_along,
        function(i) linterpolate1(
            col.m %<=% matrix(    xavail[ix], nrow=2) %-|% i,
            col.m %<=% matrix(y[-iwhere][ix], nrow=2) %-|% i,
            xwhere[i]))
}

linterpolate <- function (x, y=NULL)
        if (y %|% is.null) {
            .linterpolate(x %|% firstc, x %|% secondc) 
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

m.linterpolate <- function(x, y=NULL) {
    xy <- x %cbind% y
    xy[xy %|% secondc %|% is.na, 2] <- xy %|% linterpolate
    xy }

quadrance0 <- function(xy) rapply.(xy ^ 2, sum)

quadrances.m <- function (m, closed=F)
        if (closed) {
            m %rbind% firstr(m) %|% quadrances.m
        } else {
            rapply.(m %|% diff, sum.of.squares) }

    Doc$quadrances.m <- '
        quadrances.m returns a vector of quadrances between each
        pair of points represented by their Cartesian
        coordinates, found in the rows of the matrix argument.
        The named argument, "closed" determines whether the last
        and first rows are considered a pair of points.'

quadrances.l <- function (X, closed=F)
        quadrances.m(m.l(X, pad=0L) %|% t, closed=closed)

    Doc$quadrances.l <- '
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

irunning <- function (to, length.run=2)
        length.run %,% to %|% diff_inclusive %|% seq.int %=>%
                vapply2 %:|% 
                        (length.run %=:% "length.out" %v% seq.int)

    Doc$running <- '
        irunning returns an integer matrix whose columns contain
        length.run (arg 2) consecutive integers. Element 1 ==
        1L. Column (n + 1) == (Column n) + 1. The values in each
        column may represent the indices of data in a running
        average or similar calculation.'

running <- function (X, length.run=2)
        (capply %,% lcapply %[[b% (X %|% is.list))(
            irunning(X %|% `#`, length.run),
            `[` %<=% X)

    Doc$running <- '
        running returns a matrix or compound list containing,
        for each column or element, runs of data from the
        primary argument. Run-length is specified by arg 2.'

closed.i <- function (i) {
    if (i  %|% `#` < 2) return (i)
    if (i %|% first == i %|% last) return (i)
    i %,% i[1] }

i.closed <- closed.i #DEPRECATED USE close.i

    Doc$closed.i <- '
        closed.i returns a modified copy of the index-vector
        argument. The return is identical to the argument,
        except that the first element is repeated at the end of
        the return.
        
        Originally intended to aid in the analysis
        of closed polygons specified by a vector of indices of
        points. 

        HISTORY

        2019-11-17 changed behavior for null and single-value vectors. 
        '

i.closed.xy <- function (xy)
        xy %|% nrow %|% seq %,% 1L

pyramid <- placeholder

    Doc$pyramid <- '
        pyramid

        The numbering scheme for the edges of an n-gonal
        pyramid: base-edges are numbered counterclockwise as
        1:n; lateral edges are numbered counterclockwise
        (n+1):(2*n) where edges (n + 1) and (n + 2) belong to
        the lateral face with base-edge 1, edges (n + 2) and (n
        + 3) belong to the lateral face with base-edge 2, ...'

i.edges.pyramid.n <- function (n) {
    i <- n %|% seq + n
    rbind(i - n, i, i %|% rotate, deparse.level=0) }

    Doc$mi.lateral_faces.pyramid <- '
        mi.lateral_faces.pyramid returns a matrix of indices.
        Each column contains the indices of the edges of a
        pyramid whose base is an n-gon. See also documentation
        for pyramid.'

pyramid_edge_attributes_by_face <- function (x) {
    n <- x %|% `#` %/% 2L
    matrix(
        x[n %|% i.edges.pyramid.n %|% as.vector],
        nrow=3) }

lateral_face_quadreas.pyramid_edge_quadrances <- function (Q)
        capply(
            Q %|% pyramid_edge_attributes_by_face,
            quadrea.quadrances)

starburst <- placeholder

    Doc$starburst <- '
        a starburst is a sequence of endpoints of a starburst.
        The first point in the sequence is the central point.'

seashell <- placeholder

    Doc$seashell <- '
        A seashell is a collection of triangles whose union is a
        convex polygon. All vertices of the triangles are on the
        boundary of the polygon. Vertices are numbered
        "counterclockwise." All triangles of the seashell
        share a point in common. The common point is the first
        point of the seashell. See also Doc/seashell.pdf'

quadrances.starburst <- function (m)
        capply(
            m[-1,] %m-v% m[1,] %|% t,
            sum.of.squares)

quadrances.starburst.pt <- t %O% quadrances.starburst

    Doc$quadrances.starburst <- '
        quadrances.starburst and quadrances.starburst.pt return
        the quadrances of the starburst represented by the
        matrix-of-points (of the sort compatible with plot, etc.
        or of the one-point-per-column sort for
        quadrances.starburst.pt) argument.
        **** NOT INTENDED **** for fewer than two points.'

qareacoords <- function (points., interior.point) #TAGS quadrea area coordinates
        quadrances.m(points., closed=T) %,%
        (interior.point %rbind% points. %|% quadrances.starburst) %|%
        lateral_face_quadreas.pyramid_edge_quadrances

    Doc$qareacoords <- '
        qareacoords returns the quadrea coordinates (square-area
        coordinates) of the interior.point (arg 2) relative to
        the polygon specified by the points. (arg 1, a numeric
        matrix).  Typically, the polygon is convex and the
        interior.point is on the interior or boundary of the
        polygon. There is **** NO CHECKING **** that the points.
        are planar.'

qareacoord.ratios.rect <- function (precision) { #TAGS quadrea area coordinates
    i <- precision %|% seq0
    squares(i) / squares(precision - i) }

    Doc$qareacoord.ratios.rect <- '
        qareacoord.ratios.rect returns a double vector of ratios
        in (0, 1]. The values are ratios of quadrea coordinates
        corresponding with the opposite sides of a rectangle
        with precision (arg 1, an integer-valued numeric scalar)
        divisions along a side parallel to the altitudes of the
        corresponding triangles. Originally developed to find
        the nearest integral x- coords in a 2D space given the
        qarea coordinates.'

rect.coord.area.coords <- function (x) c(
        x[4] / x[2 %,% 4] %|% sum,
        x[1] / x[1 %,% 3] %|% sum)

    Doc$rect.coord.area.coords <- '
        rect.coord.area.coords returns the local, Cartesian
        coordinates in [0, 1] of a point within a rectangle,
        given the corresponding area coordinates (a numeric
        4-vector).
        
        The first area of the argument corresponds with the
        direction running along the first dimension. Areas of
        the argument proceed "counterclockwise" around the
        rectangle.'

rect.coord.qareacoords <- function (table, qareacoords) #TAGS quadrea area coordinates
        n.look.d(table, qareacoords %|% `/r`) %|% pred

    Doc$rect.coord.qareacoords <- '
        rect.coord.qareacoords is intended for currying. Given a
        1-D table of quadrea ratios of qarea coords of a
        rectangle of integral side length, and two complementary
        qarea coords (as a numeric vector), of returns the floor
        of the corresponding linear coord.  The table may be
        constructed using qareacoord.ratios.rect.'

rect.coords.quareacoords <- function (table, qareacoords) #TAGS quadrea area coordinates
        capply(
            matrix(qareacoords, nrow=2, byrow=T),
            rect.coord.qareacoords %<=% table,
            1)

    Doc$rect.coords.quareacoords <- '
        rect.coords.quareacoords is the 2-D equivalent of
        rect.coord.quareacoords (note the "s" or lack there of
        on "coords"). Arg 2 is a vector of ordered qarea coords,
        corresponding the "bottom," "right," "top," and "left"
        sides of a rectangle.'
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
