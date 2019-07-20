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



page <- function (a, n=1) #TAGS array 3D 3-D slice
        a[,,n]

    'page returns the n-th (arg 2) matrix of the 3-D array
    argument (arg 1).'


a.dummy <- function (dim)
        array(
            rapply(
                as_storage.mode(
                    "character",
                    arrayInd(
                        dim %|% prod %|% seq,
                        dim) %|% cflip),
                `%//%`),
              dim)

    Doc$a.dummy <- '
        a.dummy returns an array of the given dimensions.
    
        Provided that no dimension is greater than 9, each
        character string element of the return corresponds
        directly to its array index (LSB at right).

        > a.dummy(2:3)
        
            [,1] [,2] [,3]

        [1,] "11" "21" "31"

        [2,] "12" "22" "32"'

unique.a <- as.vector %O% unique

    Doc$unique.a <- '
        unique.a returns the unique values among all the
        elements of the array argument.'

dim. <- function (x)
    if (x %|% dim %|% is.null) {
        x %|% `#`
    } else {
        x %|% dim }

a. <- function ()
        array(1:24, c(2,3,4))

    Doc$a. <- '
        a. returns a 2 x 3 x 4 array with the elements 1:24.
        Intended for use in testing.'

extract.a <- function(a, list.=lapply(a %|% dim, seq), drop=T)
		do.call %<=% `[` %-|% c(
			a %|% list,
			list.,
            list(drop=drop)) 

    Doc$extract.a <- '
        extract.a is similar to `[` for arrays. The list.
        argument is a list of index vectors together
        representing a slice. Within each element of list.,
        indices may be in any order and may be repeated.
        (forward reference: offset.) Use with offset. to specify
        replacement with local coordinates.'

replace.a <- function(a, list.=lapply(a %|% dim, seq), values=a %|% as.vector)
		do.call %<=% `[<-` %-|% c(
			a %|% list,
			list., 
			recycle_len(values, vapply_ %<=% list. %-|% `#` %|% prod) %|% list)

    Doc$replace.a <- '
        replace.a is an array version of replace. The list.
        argument is a list of index vectors together
        representing a slice. Within each element of list.,
        indices may be in any order and may be repeated
        However, repetition may yield results that depend upon
        the order of assignment. (forward reference: offset.)
        Use with offset. to specify replacement with local
        coordinates.'

except.last <- function (x) {
    L <- x %|% length
    if (L < 1) x else x[-L] }

slice.big.dim <- dim %O% except.last

slice.big.length <- slice.big.dim %O% prod

l.unstack.a <- function (a) #TAGS index indices split inside outside upside down
    if (a %|% dim %|% `#` < 2) { list(a)
    } else {
        LENGTH <- a %|% slice.big.length
        lapply(
            a %|% dim %|% last %|% seq %|% pred,
            function(n) dimension(
                a[LENGTH %|% seq + n * LENGTH], #TODO FACTOR OUT LENGTH
                a %|% slice.big.dim) ) }

    Doc$l.unstack.a <- '
        l.unstack.a returns a list containing the largest
        sub-arrays of the array argument. When arg is a matrix,
        the return is a list of the columns of arg; when arg is
        a 3-D array, the return is a list of the pages of arg.
        The returns is a kind of rotated structure, in that the
        initial index corresponds to the "slowest" grouping of
        the original structure; i.e., if X[i, j, k] refers to an
        element of the original array, then Y[[k]][i, j] refers
        to the same element in the return.
        
        DETAILS

        Array storage in R is in a certain order.  indices
        proceed from fastest to slowest, left to right.
        l.unstack.a can be used to invert conceptually the order
        of indexing.'

a.stack.l <- placeholder()

MAT1 <- matrix(1)

ndim <- function (a) max(1L, a %|% dim %|% `#`)

dimension <- function (x, dimensions) {
    dim(x) <- dimensions;   x }

aapply <- function(a, FUN, FUN.VALUE=NULL, ..., USE.NAMES=T) {
    if (FUN.VALUE  % %  is.null) FUN.VALUE <- FUN(a[[1]], ...)
    dimension(vapply(a, FUN, FUN.VALUE, ..., USE.NAMES=USE.NAMES), dim(a)) }

reproduce <- function (obj, i, ...) obj[i, ..., drop=F]

subgroups.per.part <- function (dims, subgroup.size=10) {
    if (length(dims) < 1) return (0)
    nsg = ceiling(dims[1] / subgroup.size) # number of subgroups
    if (length(dims) < 2) return (nsg)
    # compute number of subgroups in each aggregation
    sed = seq(2, length(dims))
    nlin = c(nsg, sed) # preallocate
    for (j in sed) { nlin[j] = nlin[j-1] * dims[j] }
    nlin }

'
function dimension : dims array

        Returns an array that is redimensioned according to arg
        2.

function reproduce : Extract [ [[ subscript

        Similar to `[`, but the drop argument is fixed at F. The
        intention is that the return is of the same type or
        dimension as the primary argument.

        Examples:

                > reproduce(m.())
                     [,1] [,2] [,3]
                [1,]    1    3    5
                [2,]    2    4    6

                > reproduce(m.(), , 2)
                     [,1]
                [1,]    3
                [2,]    4

                # vs.
                > m.()[,2]
                [1] 3 4

                                 
function subgroups.per.part : array store dims
    Returns the number of subgroups per dimension.
    dims    the dimensions of an array.

    sgs     subgroup size, a singleton integer vector storing
            the maximum number of items per subgroup.
    
    Originally intended to facilitate reading parts of arrays
    stored as lines of text; could also be used in array
    buffering.

    Can be used to determine the skip parameter of the R
    function scan, e.g., if one wishes to skip a number of
    groups as represented in the corresponding file.
    
    An array with dimensions dims, length(dims) > 0, all(dims >
    0), is represented as a sequence of groups of subgroups.
    Within each group, each subgroup contains n items, except
    for the terminal subgroup, which may contain fewer. The
    number of items in a group is equal to dims[1].


    Example 1:

            subgroups.per.part(c(14, 3), 5)

    returns a vector equivalent to c(3, 9). A text file with
    conistently-stored values might look like this:

            1 1 1 1 1
            1 1 1 1 1
            1 1 1 1
            1 1 1 1 1
            1 1 1 1 1
            1 1 1 1
            1 1 1 1 1
            1 1 1 1 1
            1 1 1 1

    In otherwords, there are 3 subgroups for each dimension-1
    part and 9 (3 * 3) subgroups for each dimension-2 part.


    Example 2:

    Two, 12x3 matrices (12 columns, 3 rows) might be stored like this:

            1 1 1 1 1 1 1 1     # Start of Matrix 1
            1 1 1 1
            1 1 1 1 1 1 1 1
            1 1 1 1
            1 1 1 1 1 1 1 1
            1 1 1 1
            1 1 1 1 1 1 1 1     # Start of Matrix 2
            1 1 1 1
            1 1 1 1 1 1 1 1
            1 1 1 1
            1 1 1 1 1 1 1 1
            1 1 1 1

    The corresponding call would be:

            subgroups.per.part(c(12, 3), 8)

    which would return a vector equivalent to c(2, 6)


function ndim

        Returns the number of dimensions in the vector or array
        argument.

function aapply : apply

        A wrapper for vapply. However, the return is an array of
        the same dimension as the primary argument. Therefore
        FUN must return a single value.

'
