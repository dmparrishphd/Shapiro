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
# R Script

specialize("rbind", n=1) #CREATES `%rbind%`
specialize("cbind", n=1) #CREATES `%cbind%`

extend.m <- function (m) cbind(m, NA)

collar.m <- function (m) #TAGS extend
        rbind(NA, cbind(NA, m, NA), NA)

cat.row <- function (h, file="", sep=" ", append=F, row_sep=HEOL) {
        if (h  % %  `#`) cat(h, file=file, sep=sep, append=append)
        cat(row_sep, file=file, append=T) }

cat.row <- function (h, file="", sep=" ", append=F, row_sep=HEOL) {
        if (h  % %  `#`) cat(h, file=file, sep=sep, append=append)
        cat(row_sep, file=file, append=T) }

# cat.m HISTORY 2018-07-11: completely revised
cat.m <- function (m, file="", sep=" ", append=F, row_sep=HEOL)
        for (i in m  % %  colNos.m)
                cat.row(m[,i], file=file, sep=sep, append=(append || (1 < i)), row_sep=row_sep)

    Doc$cat.m <- '
        cat.m cat-s a matrix (arg 1). Rows of output (columns of the
        matrix) are separtated by row_sep (end-of-line marker by
        default). Typical usage: cat.m(t(m)); where m is a
        matrix.'

cat.tm <- function(m, ...) cat.m(m %|% t, ...)

    Doc$cat.tm <- '
        cat.tm is similar to cat.m, except that a transposed
        version of the matrix argument is output. The result is
        somewhat similar to print, insofar as the visual order of
        elements is concerned.'

m.na <- function(nrow, ncol, data=logical()) matrix(data=data, nrow=nrow, ncol=ncol)


'
TODO can we refactor something like
onnull <- function(expression., value)
    if (value %|% is.null) eval.parent(expression., 2) else value
'

cols <- function (X, j=T)
        (`[` %^% list(X, X %|% rowNos, j, drop=F))() #FUTURE: allow X to have any number of dimensions

    Doc$cols <- '
        cols returns a matrix or data frame with the specified
        columns (all columns, by default).'

cols.m <- cols #DEPRECATED use cols

l.cols <- function (m, j=m %|% colNos)
        lapply(j, cols %|% argswap, m)

    Doc$l.cols <- '
        l.cols returns a list where each element contains one of
        the columns of m (arg 1) specified by j (arg 2), in that
        order.'

rows <- function (X, i=T) (`[` %^% list(X, i, X %|% colNos, drop=F))()

rows.m <- function(m, i=NULL) cols.m(t(m), i=i)

row.r <- function (m)
        matrix(rep(rev(rowNos.m(m)), n.col.m(m)), nrow=n.row.m(m))

unique_cols <- function(m)
        matrix(m  % %  cols.m  % %  unique  % % unlist,
                nrow=n.row.m(m))

unique_rows <- function (m)
        t(unique_cols(t(m)))


min.local <- capply %O% which.min

    Doc$min.local <- '
        min.local returns the index of the function arguments
        (columns of the primary argument) which, among the sets
        of arguments given, minimize the function.

        > m

            [,1] [,2] [,3]

        [1,]    1    3    5

        [2,]    2    4    6

        > cbind(m, m/2)

            [,1] [,2] [,3] [,4] [,5] [,6]

        [1,]    1    3    5  0.5  1.5  2.5

        [2,]    2    4    6  1.0  2.0  3.0

    > min.local(cbind(m, m/2), sum)

    [1] 4' 


.crwhich <- function (FUN, m)
        FUN(m != 0, which1) %|% un(is.na)
cwhich <- .crwhich %<=% capply
rwhich <- .crwhich %<=% rapply

    Doc$cwhich <- '
        cwhich tells which columns of the matrix argument have
        nonzero values.'

    Doc$rwhich <- '
        rwhich tells which rows of the matrix argument have
        nonzero values.'

lcapply  <- function (m, FUN, ...)
        lapply(
            m %|% colNos,
            function (i, m, ...) FUN(m[, i], ...),
            m,
            ...)

colply <- capply #function #DEPRECATED. use capply

inapply <- function (X, FUN, FUN.VALUE=NULL, offsets=ic(0, 1), ...)
        colply(iseqs(seq_along(X), offsets)  % %  t,
               FUN, FUN.VALUE=FUN.VALUE, X, ...)

colply.matched <- function (m, FUN, ...) {
    argn <- items.named(list(...))
    argu <- items.unnamed(list(...))
    l <- list()
    for (i in m  % %  colNos.m) l <- l %,% list(do.call(
            FUN, list(m[, i]) %,% (argu %[mod% i) %,% argn))
    l }

vapply.FUN.VALUE.typ <- function (X, ...) { "PLACEHOLDER" } 

vcolply.matched <- function (m, FUN, FUN.VALUE, ...) {
    argn <- items.named  (list(...))
    argu <- items.unnamed(list(...))
    mm <- matrix(rep(FUN.VALUE, m  % %  ncol), ncol=m  % %  ncol)
    for (i in m  % %  colNos.m) {
        mm[, i] <- do.call(
            FUN, list(m[, i]) %,% (argu %[mod% i) %,% argn) }
    mm }

rowply <- function (m, FUN, ...) colply(t(m), FUN, ...)

vcolply <- function (m, FUN, FUN.VALUE, ..., USE.NAMES=T)
        vapply(m  % %  colNos.m,
               function (x, ...) FUN(col.m(m, x), ...),
               FUN.VALUE,
               ...,
               USE.NAMES=USE.NAMES)

m.primate = function (m) { nc = colNos.(m);   nr=rowNos.(m);
    warning("m.primate untested since revision of colply and rowply.")
    .('returns parallel vectors that together locate the first and last T value in each row and column.')
    list(   c(colply(m, primate), colply(m, primate.r), nr, nr),
            c(nc, nc, rowply(m, primate), rowply(m, primate.r))
    )
}

block.extract.m <- function(m, rows, cols, na=NA)
        matrix(
           capply(
                matrix(
                    capply(m, block.extract, , rows[1], rows[2], na),
                    nrow=rows  % %  dimension.irange)  % %  t,
                block.extract, , cols[1], cols[2], na)  % %  t,
           ncol=cols  % %  dimension.irange)

v.m <- function(FUN, m, byrow=F, ...)
        jump.b(byrow, list(capply, rowply), m, FUN, ...)

all.m   <- curry(v.m, all)
any.m   <- curry(v.m, any)
count.m <- curry(v.m, count.v) # function FUTURE: ADD PARAMETER count.na TO TELL WHETHER TO COUNT NA VALUES
max_m   <- curry(v.m, max)
min_m   <- curry(v.m, min)
prod.m  <- curry(v.m, prod)
sum.m   <- curry(v.m, sum)
' FUTURE:
minloc.m
maxloc.m
'

clamp <- function(x, min_=0L, max_=1L)
        min_m(rbind(max_, max_m(rbind(min_, x))))

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

stipple <- function (m, ijz) {
    m[ijz[,1:2, drop=F]] <- ijz[,3]
    m }

    Doc$stipple <- '
        stipple returns a copy of the primary matrix argument,
        except that the elements specified by the first two
        columns of the secondary matrix have the values of the
        corresponding rows of the third column of the secondary
        matrix.
        
        > stipple(matrix(0, nrow=4, ncol=4), matrix(c(4, 2, 42), ncol=3))
             [,1] [,2] [,3] [,4]
        [1,]    0    0    0    0
        [2,]    0    0    0    0
        [3,]    0    0    0    0
        [4,]    0   42    0    0'

m.ijz <- function (ijz, nrow=NULL, ncol=NULL, default=NA)
        stipple(
            matrix(
                as.vector(default, mode=ijz %|% typeof),
                nrow=
                    if (nrow %|% is.null) {
                        ijz[,1] %|% max
                    } else { nrow },
                ncol=
                    if (ncol %|% is.null) {
                        ijz[,2] %|% max
                    } else { ncol } ),
            ijz)

m.ij <- function(ij, ...) m.ijz(cbind(ij, T), ...)

.spread.m <- function (m, sep=NA, ...)
{   nr <- n.row.m(m)
    matrix(spread.l(m  % %  cols.m, sep=rep(sep, nr), ...)  % %  unlist, nrow=nr) }

spread.m <- function (m, sep=NA, byrow=F, ...)
        if (byrow) t(.spread.m(t(m), sep, ...)) else .spread.m(m, sep, ...)

spread2.m <- function (m, sep=NA, ...)
        spread.m(spread.m(m, sep=sep, byrow=T, ...), sep=sep, ...)

ij.m <- function (m) {
    wh <- which.nonzero(m);  matrix(col(m)[wh] %,% row.r(m)[wh], ncol=2) }

ij.im <- function (m) {
    wh <- which.nonzero(m);  matrix(row(m)[wh] %,%   col(m)[wh], ncol=2) }

indices <- function (nrow, ncol)
        cbind(ncol  % %  seqN, rep(nrow  % %  seqN, ncol)  % %  sort)

flipx.xy <- function (xy) cbind( xy[,1], -xy[,2])
flipy.xy <- function (xy) cbind(-xy[,1],  xy[,2])

flipx <- flipx.xy #DEPRECATED use flipx.xy
flipy <- flipy.xy #DEPRECATED use flipy.xy

flip.xy <- function (xy, axis="x")
        (if (axis == "x") flipx.xy else flipy.xy)(xy)

cflip <- function(m)
        m[,m %|% colNos %|% rev, drop=F]

rflip <- function(m)
        m[m %|% rowNos %|% rev,, drop=F]

crflip <- cflip %O% rflip


'
function flipx
function flipy
function flip

        The flip funcitons return the reflection of their
        arguments, which are matrices of x, y pairs (x in the first
        column and y in the
        second.)

function centroids : finite difference regular

        Given a matrix of indices (one column per dimension) of
        cells in a regular grid, returns the "indices"
        corresponding to the centroids of each cell. If only one
        dimension is given, the argument may be a vector.

        The return may be scaled and shifted to align with a
        regular geographic grid.

        Example:

                > indices(2, 2)  % %  centroids.grid
                     [,1] [,2]
                [1,]  0.5  0.5
                [2,]  1.5  0.5
                [3,]  0.5  1.5
                [4,]  1.5  1.5

function scale : column

        Returns a matrix of the same dimension as the primary
        argument, with values equal to those of the matrix
        columns, scaled according to the columnar scale factors
        (optinal argument scale.factors).

        Scale factors will be reycled if their number is less
        than the number of columns in the matrix.

        Intended uses:

                For scaling x,y values for plotting

                For the generation of regular grids

function indices : matrix

        Returns a two-column matrix where each row contains the
        row/column index of a matrix having the number of rows
        (arg 1) amd columns (arg 2) given. The indices are in
        the same order as the storage order of a corresponding
        matrix.

                > indices(2, 2)
                     [,1] [,2]
                [1,]    1    1
                [2,]    2    1
                [3,]    1    2
                [4,]    2    2

function ij.im : cartesian index indices cells image

        See also ij.m

        Returns the i- and j-coords of nonzero values within the
        matrix m (arg 1).  The first column corresponds to i=1,
        while the first row corresponds to j=1, consistent with
        the way matrices are image-d in R.

function ij.m : cartesian index indices cells ; old name xy.m

        HISTORY 2018-05-29: changed to yield a 2-column matrix
        rather than a list.

        See also ij.im

        Returns the i- and j-coords of nonzero values within the
        matrix m (arg 1).  The first column corresponds to i=1,
        while the LAST row corresponds to j=1, consistent with
        the way matrices are print-ed in R.

        Somewhat the opposite of m.cart.

function v.m :

        

function m.ijz : OLDNAME m.cart : cartesian

        Given a matrix or data frame where columns 1 and 2
        contain row and column indices into a matrix to be
        created and column 3 contains data that will be placed
        in the new matrix.

        The values in columns 1 and 2 may be either integer or
        integer-valued doubles.

        It is an error to have "indices" less than 1.

        HISTORY 2018-02-21: completely rewritten

function m.ij

        Similar to m.ijz, but requires only an ij matrix.
        z-values are assumed TRUE.

function all.m
function any.m
function count.m
function max_m
function min_m
function prod.m
function sum.m

        Compute all, any, count, max, min, prod, or sum along
        columns or vectors of a matrix.


function colply.matched : lapply

        Similar to colply. Operates on the columns of a matrix m
        (arg 1). The secondary argument, FUN is a function of a
        COLUMN of the matrix (NOT of its elements) and
        additional arguments. Internally, FUN is called once for
        each column of the matrix (arg 1). The unnamed
        additional arguments, parsed from ..., are passed to FUN
        according to their order, where the first unnamed
        argument matches the first column, the second unnamed
        argument matches the second column, and so
        on---arguments will be recycled if thier number is less
        than the number of columns. All additional named
        arguments are passed to FUN each time FUN is called.
        Examples:

                > # to scale column 1 by a factor of 2 and
                > # column 2 by a factor of 3
                > colply.matched(matrix(rep(1, 4), ncol=2), `*`, 2, 3)
                [[1]]
                [1] 2 2

                [[2]]
                [1] 3 3
                > # sumilarly, to add an additional shift at the end
                > colply.matched(matrix(rep(1, 4), ncol=2),
                +     function(x, fact, shift) fact * x + shift,
                +     2, 3, shift=.1)
                [[1]]
                [1] 2.1 2.1

                [[2]]
                [1] 3.1 3.1

function vcolply.matched : lapply

        Similar to colply.matched and vapply: a third parameter
        specifies the type and length of the vector return of
        each call to FUN. Thus, the size and shape of the return
        is determined ahead of time, as in vapply. The return of
        vcolply.matched is a matrix having the same number of
        columns as the primary argument, and a number of rows
        equal to the length of FUN.VALUE.  Examples:

                > vcolply.matched(matrix(rep(1, 4), ncol=2), `*`, c(0, 0), 2, 3)
                     [,1] [,2]
                [1,]    2    3
                [2,]    2    3
                > vcolply.matched(matrix(rep(1, 4), ncol=2), function(x, fact, shift) fact * x + shift, c(0, 0), 2, 3, shift=.1)
                    [,1] [,2]
                [1,]  2.1  3.1
                [2,]  2.1  3.1



function rowply : lapply

        Similar to lapply, but operates on the rows of a
        matrix.

function unique_cols : matrix columns

        Returns a matrix consisting of the unique columns of the
        matrix argument.

function unique_rows : matrix rows

        Returns a matrix consisting of the unique rows of the
        matrix argument.

function extend.m : augment

        Returns a modified copy of the matrix argument. The copy
        has an additional column of NA values at right.

function collar.m : frame map collar extend augment

        Returns a modified copy of the matrix argument. The copy
        can be thought of as the original with a frame or collar
        of NA values wrapped around it on all sides.

        Originally intended for plotting multilayer images.

function cat.row

        cat-s the vector (arg 1) elements, followed by a row
        separator, row_sep, which is the end-of-line marker by
        default.

        Originally intended to write formatted data one row at a
        time, where one row maps to one line of a file (possibly
        stdout)


function inapply

        Applies FUN (arg 2) to the INterior (hence INapply) of
        the X (arg 1), typically a vector or list. FUN is a
        function of indices into X and X (in that order).

        Example. Central differencing of a vector might be
        accomplished as:

                > inapply(
                +     c(1, 4, 9, 25),
                +     function(i, x) x[i[2]] - x[i[1]],
                +     offsets=ic(0, 2))
                [1]  8 21

function block.extract.m

        Similar to block.extract, but for matrices

function seqnD : multi-dimensional sequence

        Returns a sequence of multi-dimensional values as a
        matrix, one row per value. Values begin at (1, 1, ...)
        and progress according to the dims argument. Example:
                > seqnD(c(2, 3))
                     [,1] [,2]
                [1,]    1    1
                [2,]    2    1
                [3,]    1    2
                [4,]    2    2
                [5,]    1    3
                [6,]    2    3
'
