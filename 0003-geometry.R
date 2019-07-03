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



rat.spreads.lines <- pairwise %<=% rat.spread

    Doc$rat.spreads.lines <- '
        rat.spreads.lines returns a matrix of spreads, one for
        each pair of lines in the lines argument, one spread per
        column of the return. The order of the spreads is
        consistent with the order of the points of the lines
        argument (i.e., the first spread is that between line 1
        and line 2).'

Doc$point <- '
    A point is a vector of values or an n x 1 matrix with one
    value per dimension, in that order.'

Doc$points <- '
    A collection of points is a matrix of point-s, one point per
    column. The transpose of a collection of points may be
    plotted with the R functions plot, points, etc.' 

min_pt <- rapply %|% argswap %<=% min #TAGS lower left corner bounding box origin
max_pt <- rapply %|% argswap %<=% max #TAGS upper right corner bounding box
bb.pt <- function (points) #TAGS bounding box
        applyf(points, min_pt %,% max_pt) %|% rbind_l

pg.bb <- function (bb) #TAGS bounding box polygon
        bb["1 3   2 3   2 4   1 4" %|% i.h] %|% matrix2r

bb.bb <- function (bounding.box.list)
        capply(
            do.call(rbind, bounding.box.list),
            range)

ipt.bb <- function (bb, .ibb=capply1to1(bb, ceiling %,% floor)) #TAGS points bounding box
        .ibb %|% diff_inclusive %|% m.indices %|% pred %|% t + .ibb[1,]

    Doc$ipt.bb <- '
        ipt.bb returns the integer-valued points within
        (inclusive) the bounding box given. 
    
        > m

        [,1] [,2]

        [1,]   -1    1

        [2,]   -1    1

        > ipt.bb(m)

             [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]

        [1,]   -1    0    1   -1    0    1   -1    0    1

        [2,]   -1   -1   -1    0    0    0    1    1    1'

dimnames_bb <- function (bb)
        list("MIN" %,% "MAX", prefix(bb %|% colNos, "DIM"))

with.dimnames.bb <- function (bb)
        rename.all.dims(bb, bb %|% dimnames_bb)

justify.pt <- function (m) #TAGS translate snap move origin transformation
        capply(m, `-` %|% argswap %<=% (m %|% min_pt))

    Doc$min_pt <- '
        min_pt returns the min values along each dimension of
        the points argument.'

    Doc$max_pt <- '
        max_pt returns the max values along each dimension of
        the points argument.'

    Doc$bb.pt <- '
        bb.pt retuns the bounding box of the points argument,
        one column per dimension.'

    Doc$bb.pt <- '
        bb.bb retuns the bounding box of the bounding boxes
        contained in the list argument; all bounding boxes
        should have the same dimensions.'

    Doc$justify.pt <- '
        justify.pt returns a modified copy of the points
        argument. The return represents a translation of the
        points so that the minimum coordinates in each direction
        are zero. Thus the points are justified to the
        coordinate axes.'

which.pt.in.pts <- function (points, point)
        capply(
            points %m==v.% point,
            all) %|% which

