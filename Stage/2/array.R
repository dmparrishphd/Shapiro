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

i.cube <- function(center, delta=0L) { #TAGS index array contiguous #RELATED block.extract.a
    center <- center %|% ifloor
    delta <- delta %|% ifloor
	DELTA <- delta %|% succ
	HI <- DELTA %|% odd
	NDIM <- center %|% `#`
	Return <- crep(c(1, HI), NDIM) %m+v% center - DELTA
	colnames(Return) <- prefix(1:NDIM, "D")
	Return }

    Doc$i.cube <- '
        conceptually, i.cube returns a matrix of indices
        representing a cube centered at center, with delta
        indices on either "side" of the center (and side length
        2 * delta + 1).
        
        **** ONLY THE FIRST AND LAST **** index on either "side"
        of the center are represented.
    
        Originally intended to be used with `block.extract.a`.'

`r+` <- `+` %|% argswap %=>% rreflexive

    Doc$`r+` <- '
        `r+` returns a matrix with one fewer rows than the
        matrix argument. The rows of the return are the sums of
        the rows of the argument matrix. Return row 1 ==
        argument row 1 + argument row 2, etc.'

frame. <- function (a, nmar, data=NA) #TAGS assign border margin pad extend
        replace.a.at( 
            array(data=data, dim=nmar %|% `r+` + dim(a)),
            a,
            nmar[1,] %|% succ)

    Doc$frame. <- '
        frame. returns a modified copy of the array argument
        (arg 1). The return contains additional elements that
        pad the original elements with the number of elements
        specified by the nmar argument (arg 2).
        
        The nmar argument is a two-row matrix with the number of
        columns equal to the number of dimensions of the array
        argument (arg 1). The top row tells how many elements to
        pad on the lower end of the dimension, and the top row
        tells how many elmements to pad on the upper end.

        > m

             [,1] [,2]

        [1,]    1    3

        [2,]    2    4

        > frame.(m, matrix(c(1,2,3,4), ncol=2))

             [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]

        [1,]   NA   NA   NA   NA   NA   NA   NA   NA   NA

        [2,]   NA   NA   NA    1    3   NA   NA   NA   NA

        [3,]   NA   NA   NA    2    4   NA   NA   NA   NA

        [4,]   NA   NA   NA   NA   NA   NA   NA   NA   NA

        [5,]   NA   NA   NA   NA   NA   NA   NA   NA   NA
        '

cardinal.unit.vectors <- function(ndim=2) {
    m <- ndim %|% m.identity
    rbind(m, -m) }

    Doc$cardinal.unit.vectors <- '
        cardinal.unit.vectors return a relative matrix-index
        (see documentation for `[`) for an array of dimension ndim
        (arg 1).

        > cardinal.unit.vectors()

            [,1] [,2]

        [1,]    1    0

        [2,]    0    1

        [3,]   -1    0

        [4,]    0   -1'

.neighbors <- function (i)
        i %|% `#` %|% cardinal.unit.vectors %m+v% i

every.neighbor.mi <- function (mi) { #TAGS index matrix index array
    U <- mi %|% ncol %|% cardinal.unit.vectors
    vapply(
        mi %|% rowNos,
        function(k) U %m+v% mi[k,],
        array(NA_integer_, U %|% dim) ) }

    Doc$every.neighbor.mi <- '
        every.neighbor.mi returns an array of index matrices
        into a hypothetical array of indefinite extent
        (especially including indices 0L and any or all of
        dim(array) + 1L).  There is one such index matrix
        (dimensions 1 and 2 of the return) for each matrix index
        of the index matrix argument: rows of the index matrix
        argument correspond with "pages" (dimension 3) of the
        return.'

    Doc$.neighbors <- '
        .neighbors returns an index matrix into a hypothetical
        array with the valid dimensional index i (arg 1).

    > .neighbors(c(1,2))

        [,1] [,2]

    [1,]    2    2

    [2,]    1    3

    [3,]    0    2

    [4,]    1    1'

.sub.index <- function (i, dims=NULL)
        any(i < 1L)

sub.index <- function (i, dims=NULL) #TAGS array
        rapply.(i, .sub.index, , dims)

    Doc$sub.index <- '
        sub.index returns a logical vector indicating which of
        the candidate matrix-indices in i (arg 1) have at least
        one component that cause the index to be invalid for an
        array with dims dims (arg 2) on account of being too low.
        E.g., the index 0L is not valid, because 0L < 1L. It is
        not necesary to specify the dims argument, as all arrays
        are assumed to be indexed from 1.'

.super.index <- function(i, dims)
        any(i > dims)

super.index <- function (i, dims) #TAGS array
        rapply.(i, .super.index, , dims)

    Doc$super.index <- '
        super.index returns a logical vector indicating which of
        the candidate matrix-indices in i (arg 1) have at least
        one component that cause the index to be invalid for an
        array with dims dims (arg 2) on account of being too
        high.  E.g., the index 2L is not valid for a 1-vector,
        because 2L > 1L.'

meta.index <- function (i, dims) #TAGS array
        sub.index(i, dims) | super.index(i, dims)

    Doc$meta.index <- '
        meta.index returns a logical vector indicating which of
        the candidate matrix-indices in i (arg 1) have at least
        one component that cause the index to be invalid for an
        array with dims dims (arg 2) on account of being beyond
        the valid range for a corresponding array. E.g., the
        index 2L is not valid for a 1-vector, because 2L > 1L.'

neighbors <- function (i, dims) { #TAGS index array
    k <- .neighbors(i)
    k[!meta.index(k, dims),] }

    Doc$neighbors <- '
        neighbors returns the valid array indices (as a matrix
        index) that are neighbors of the cell with dimensional
        index i in the matrix of dims dims.'

m.neighbors.m <- function(m, dims)
        do.call(
            rbind,
            lrapply(
                m,
                neighbors %|% argswap %<=% dims)) %|%
        unique

	Doc$m.neighbors.m <- '
        m.neighbors.m returns an index matrix of the unique
        neighbors of the matrix-cells whose matrix indices are
        given by arg 1 and whose dimensions are given by arg 2.'

pbind <- function (mm) {
    x <- c(mm, recursive=T)
    dims <- dim_complete(x %|% `#`, mm[[1]] %|% dim)
    y <- vector(mode=x %|% typeof, length=dims %|% prod)
    y[1:(x %|% `#`)] <- x
    dimension(y, dims) }

    Doc$pbind <- '
        pbind is similar to cbind or rbind; pbind is designed to
        combine matrices into a 3D array.
        
        The argument is a list of arrays and/or vectors. The
        first item in the list determines the lower dimensions
        of the return, and the highest dimension of the return
        is large enough so that the return may contain all of
        the values found at the leaves of the list argument
        (internally the recursive argument of `c` is set to
        TRUE).

        Any elements in the return that are present merely to
        accomodate the dimension have the default value
        corresponding to the type of the return (i.e., FALSE,
        0L, 0, 0+0i, "")
        
        Example:
        
        > M <- matrix(1:4, ncol=2)

        > M

        #        [,1] [,2]

        #   [1,]    1    3

        #   [2,]    2    4

        > pbind(list(M)) # NOTE THE DIMENSION OF THE RETURN

        #   , , 1

        #        [,1] [,2]

        #   [1,]    1    3

        #   [2,]    2    4

        > pbind(list(M, M))

        #   , , 1

        #        [,1] [,2]

        #   [1,]    1    3

        #   [2,]    2    4

        #   , , 2

        #        [,1] [,2]

        #   [1,]    1    3

        #   [2,]    2    4

        > pbind(list(dimension("AS  YOU WISH", 1), TRUE, "LOVE")) %|% t

             [,1]
        [1,] "AS  YOU WISH"
        [2,] "TRUE"
        [3,] "LOVE"
        
        '
