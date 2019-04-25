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
########################### fin-diff ###########################
# functions related to Finite Differences

.scale <- function (FUN, m, ...) vcolply.matched(
    m, FUN, vector(length=m  % %  nrow), ...)
                                                 
scale <- .scale %<=% `*` # function

shift_euclid.m <- .scale %<=% `-` # function
datum.shift <- shift_euclid.m # function

grid.centers <- function (nrow, ncol, xmin=0, ymin=0, dx=1, dy=dx)
        datum.shift(
            scale(indices(nrow, ncol)  % %  centroids, dx, dy),
                    -xmin, -ymin)

grid.lattice.points <- function (ncol, nrow)
        indices(1L + nrow, 1L + ncol) - 1L

    Doc$grid.lattice.points <- '
        Given the number of columns and rows in a 2D regular
        grid, returns the Cartesian coordinates of its lattice
        points.  The scale in both directions is 1 unit. The
        return may be used to produce transformed coordinates
        (e.g., via scaling).'

diff.fwd <- function(v) c(v[-1], NA) - v

diff.bak <- function (v) -rev(diff.fwd(rev(v)))

diff.central <- function (v) c(NA, v[-2:-1] - v[1:(v  % %  `#` - 2)], NA)

diff.fwd.m <- function (m, byrow=F)
        if (byrow) t(diff.fwd.m(t(m))) else cbind(m[,-1], NA) - m

diff.central.m <- function (m, byrow=F)
        if (byrow) t(diff.central.m(t(m))) else
            cbind(NA, m[,-2:-1] - m[,c(-ncol(m) + 1, -ncol(m))], NA)

.reflex <- function (FUN, v) FUN(v[-1], v[-length(v)])
.reflex.m <- function (FUN, m, byrow=F)
        if (byrow) FUN(m[-1,], m[-nrow(m),]) else FUN(m[,-1], m[,-ncol(m)])

orr  <- .reflex %<=% `|`
xorr <- .reflex %<=% xor
equalr <- .reflex %<=% `==`

i.edges.b <- xorr %O% which

i.edge1.b <- xorr %O% which1

    Doc$i.edges.b <- '
        i.edges.b returns the indices of the logical vector
        argument where the next element of the vector contains a
        different value. In this context, NA-values are not
        considered to be different from any other logical
        value.'

    Doc$i.edge1.b <- '
        i.edge1.b is similar to i.edges.b, except that the
        index of only the first edge is returned. If there are
        no edges, NA is returned.'

orr.m  <- .reflex.m %<=% `|` # function
xorr.m <- .reflex.m %<=% xor # function

c.grad.m <- function(m)
        matrix(
            complex(
                length.out=m  % %  `#`,
                real=diff.central.m(m, byrow=T),
                imaginary=m  % %  diff.central.m
            ),
            ncol=ncol(m))

interlace.m <- function (m1, m2) t(matrix(t(cbind(m1, m2)), nrow=ncol(m1)))

interlace.m <- function (m1, m2, byrow=F)
        if (byrow) t(interlace.m(t(m1), t(m2))) else
                matrix(rbind(m1, m2), nrow=nrow(m1))

lattice.empty <- function (mrow, mcol, empty=NA)
        matrix(empty, nrow=2L * mrow + 1L, ncol=2L * mcol + 1L)

lattice.empty.m <- function (m) {
     }

lattice.cols <- function (m) {
    n <- cbind(NA, NA, interlace.m(m, NA + m), NA)
    rbind(NA, interlace.m(n, NA + n, byrow=T)) }

lattice.rows <- function (m) {
    n <- rbind(NA, NA, interlace.m(m, NA + m, byrow=T), NA)
    cbind(NA, interlace.m(n, NA + n)) }

lattice.rank.and.file <- function(File, Rank) {
    #HISTORY 2018-07-25: reworked to accomodate simplified overlay
    overlay.m(lattice.cols(File), lattice.rows(Rank))
}

lattice.xorr.m <- function (m) {
    lattice.rank.and.file(xorr.m(m), xorr.m(m, byrow=T))
}

'
    m <- lattice.xorr.m(matrix(c(rep(F, 10), rep(T, 5), rep(F, 10)), byrow=T, nrow=5))
    for (j in evens((ncol(m) - 1L) %/% 2L)) {
        for (i in evens((nrow(m) - 1L) %/% 2L))
            print(c(i, j))

    nr <- (nrow(m) - 1L) %/% 2L
    nc <- (ncol(m) - 1L) %/% 2L
    cbind(evens(nr), sort(rep(evens(nc), nr)))

    }

'
lattice.diff.fwd.m <- function (m)
{   l <- lapply(list(
            diff.fwd.m(m, byrow=T),
            diff.fwd.m(m, byrow=F)), spread2.m)
    l[[1]] <- cbind(rbind(NA, l[[1]]), NA)
    l[[2]] <- rbind(cbind(NA, l[[2]]), NA)
    o <- overlay.m(l[[1]], l[[2]])
            #HISTORY 2018-07-25 reworked to accomodate new
            #definition of overlay.m
    o[1:(n.row.m(o)-2), 1:(n.col.m(o)-2)]
}

interior.boundaries.im <- function (m)
        m  % %  lattice.diff.fwd.m  % %  monochrome.im

stagger <- function (d, FUN=mean)
        unlist(lapply(matrix(stepsx(d), ncol=2, byrow=T)  % %  rows.m, FUN))

refine <- function (d, FUN=formals(stagger)$FUN)
        sort(d %,% stagger(d, FUN))

image_par.x.lattice.mx <- function (x, FUN=formals(stagger)$FUN)
        stagger(refine(x, FUN), FUN)

image_par.y.lattice.my <- image_par.x.lattice.mx

.image_lattice.im <- function (
        im, immxy, parFUN, refineFUN=formals(stagger)$FUN)
{   if (immxy  % %  is.null) parFUN(im) -> immxy
    image_par.x.lattice.mx(immxy, FUN=refineFUN) }

image_lattice.im <- function (
        iml, imm=NULL, immx=NULL, immy=NULL, refineFUN=formals(stagger)$FUN, ...)
{   if (imm  % %  is.null)
            if ((immx  % %  is.null) || (immy  % %  is.null))
                    return (NULL)
    immx <- .image_lattice.im(
            imm, immx, image_par.x.reg, refineFUN=refineFUN)
    immy <- .image_lattice.im(
            imm, immy, image_par.y.reg, refineFUN=refineFUN)
    image_im(iml, immx, immy, ...)
}

'
function lattice.m : matrix

        Returns an "empty" lattice matrix corresponding to a
        matrix whose size is given by the number of rows (arg 1)
        and columns (arg 2).

function *lattice*

        For a given a raster matrix with nr rows and nc columns, the corresponding a lattice matrix
has 2 * nr + 1 rows and 2 * nc + 1 columns. A lattice
matrix may store values associated with the centroids, edges,
and corners of each cell in the corresponding raster
matrix, where interior edges are shared between two raster
cells, and interior corners are shared among four raster cells.


Consider the matrix returned by m.:
> m.()
     [,1] [,2] [,3]
[1,]    1    3    5
[2,]    2    4    6
> 



whose elements
        indicate whether the corresponding edge of the given
        matrix are iterior boundaries (i.e., have different
        values between the corresponding pair of elements).
        Example:

                > interior.boundaries.im(matrix(c(1,1,1, 2,2,2), nrow=2, byrow=T))
                     [,1] [,2] [,3] [,4] [,5]
                [1,]    0    0    0    0    0
                [2,]    1    0    1    0    1
                [3,]    0    0    0    0    0

                #    1 2 3 4 5
                #
                # 1  1 | 1 | 1
                # 2  --+---+---
                # 3  2 | 2 | 2


function refine  : grid refinement finite difference
function stagger : grid refinement finite difference

        Given a vector (arg 1) representing the position of grid
        lines along one dimension and an optional argument FUN,
        where FUN has been used to determine the positon of the
        new gridlines. FUN is a function of the two original
        gridlines for which an intermediate gridline is being
        determined. Returns a vector representing a...
        
                (refine)
        
                        refined grid. Example:

                        > refine(refine(0:1))
                        [1] 0.00 0.25 0.50 0.75 1.00

                (stagger)

                        staggered grid. Example:

                        > stagger(0:2)
                        [1] 0.5 1.5
                        
function image_par.x.lattice.mx
function image_par.y.lattice.my
        
        Returns the x-parameter for use with image-ing a lattice
        of a given base grid. The value is computed from the
        vector argument represeting the grid lines of the base
        grid. Exmple:

                > image_par.x.lattice.mx(0:3)
                [1] 0.25 0.75 1.25 1.75 2.25 2.75

function v.diff.fwd.v

        Returns the forward difference of the vector argument.

function v.diff.bck.v

        Returns the backward difference of the vector argument.

function m.lattice.diff.fwd.m : forward difference matrix

        m.lattice.dif.fwd.m returns a lattice containing the
        forward differences in both directions

function interior.boundaries.im

        Given a matrix, returns a lattice matrix whose elements
        indicate whether the corresponding edge of the given
        matrix are iterior boundaries (i.e., have different
        values between the corresponding pair of elements).
        Example:

                > interior.boundaries.im(matrix(c(1,1,1, 2,2,2), nrow=2, byrow=T))
                     [,1] [,2] [,3] [,4] [,5]
                [1,]    0    0    0    0    0
                [2,]    1    0    1    0    1
                [3,]    0    0    0    0    0

                #    1 2 3 4 5
                #
                # 1  1 | 1 | 1
                # 2  --+---+---
                # 3  2 | 2 | 2

function image_lattice.im

        Displays the indexed color image matrix (arg 1) which
        represents the lattice of another "base grid" image
        matrix (optional imm arg).  Optionally, the x- and
        y-coords of the grid lines of the base grid may be
        specified (immx and immy optional args)---if not, then
        the base grid must be specified. The dot-dot-dot
        argument is passed to image_im, which may pass some
        arguments on to image.

        Both arg 1 and the optional base grid (if present),
        should be in the same order as expected by image. The
        im.v and im.m functions may be used to reorganize vector
        or matrix data from "English" order.

function diff.fwd.m : finite difference forward
function diff.central.m : finite difference central

        Returns the forward or central difference of columns
        (i.e., difference between columns) of the matrix
        argument when the optional byrow argument is the default
        value of F.  Use a byrow argument of T to compute
        forward differences between rows.

function c.grad.m

        Computes the gradient of the values in the matrix
        argument using central differencing. Returns the result
        as a complex matrix where the real part is the by-row
        ("faster", along column) component and the imaginary
        part is the by-column ("slower", along row) component.

function xorr : EOR XOR Exclusive Or

        Reflexive xor

        Returns a logical vector whose values are determined by
        xor-ing all pairs of adjacent values found in the
        logical vector argument.

        The result of appending NA to the front or back of the
        return can be thought of as a backward or forward
        difference, of sorts.

function xorr.m : EOR XOR Exclusive Or

        Reflexive xor (for matrices)

        See also xorr

        Returns a logical matrix whose values are determined by
        xor-ing adjacent columns or rows, depending upon the
        optional byrow argument, which defaults to F.

        Originally developed for raster contouring.

        Examples:

                > m
                      [,1]  [,2]
                [1,] FALSE FALSE
                [2,] FALSE  TRUE
                [3,]  TRUE FALSE
                [4,]  TRUE  TRUE
                > # dimension "corrects" the demotion to vector
                > dimension(xor.m(m), c(4, 1))
                    [,1]
                [1,] FALSE
                [2,]  TRUE
                [3,]  TRUE
                [4,] FALSE
                > xor.m(m, byrow=T)
                    [,1] [,2]
                [1,] FALSE TRUE
                [2,]  TRUE TRUE
                [3,] FALSE TRUE

function .scale

        Helper function / work horse for scale. Can be used to
        define similarly-behaved functions, such as
        shift_euclid.m.
        
function scale : multiply

        Returns a modified copy of the primary marix argument,
        where each of the columns have been multiplied by the
        corresponding positional argument argument beyond the
        primary argument. Scale factors are recycled if
        necessary to produce the number of factors equal to the
        number of columns.

        The typeof the return is determined by the types of the
        inputs and the operation of `*`.

        Anticipated usages:
        
                scaling plot-s "x" matrix for plotting.

                generation of coordinates related to finite
                difference grids.

        Examples:

                > scale(matrix(1L), 1.5)
                     [,1]
                [1,]  1.5
                > # The following could be centroids of grid
                > # cells of a finite difference grid with
                > # different spacing in each direction.
                > scale(centroids(indices(2, 2)), 1000, 500)
                     [,1] [,2]
                [1,]  500  250
                [2,] 1500  250
                [3,]  500  750
                [4,] 1500  750

function datum.shift : subtract translate
function shift_euclid.m : subtract translate

        Similar in operation to scale, but subtracts the
        additional positional arguments from the columns of the
        primary argument.

        Can be used to model a geometric translation to the
        "left", or a datum shift. Example:

                > m.()  % %  t
                    [,1] [,2]
                [1,]    1    2
                [2,]    3    4
                [3,]    5    6
                > datum.shift(m.()  % %  t, 3, 4)
                    [,1] [,2]
                [1,]   -2   -2
                [2,]    0    0
                [3,]    2    2

function grid.centers

        Given the parameters of a regular, 2D grid, returns a
        matrix where column 1 contains the x-coords and column 2
        contains the y-coords of the centroids of the respective
        grid.



function interlace :

        Returns a vector formed from several vector arguments.
        The elements from each of the several vectors are chosen
        in turn, for index 1, 2, and so forth, with recycling if
        needed. Example:

                > interlace(0, 1:2, 11:12, 21:22)
                [1]  0  1 11 21  0  2 12 22

'
