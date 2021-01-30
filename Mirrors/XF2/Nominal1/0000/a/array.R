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



`%[a%` <- function(x, a) #TAGS extract array
		dimension(x[a %|% as.vector], a %|% dim)

	Doc$`[a%%` <- '
		`%[a%` extracts elements from arg 1
		by taking the elements specified by the elements
		of arg 2. The return has the same shape as arg 2.

		> -1:1 %[a% matrix(c(2,1,1, 3,2,1, 3,3,2), nrow=3)

			[,1] [,2] [,3]
		[1,]    0    1    1
		[2,]   -1    0    1
		[3,]   -1   -1    0'

stipple3 <- function(mi, z, .dim) { #TAGS array populate
	a <- array(NA + z[1], .dim)
	a[mi] <- z
	a }

    Doc$stipple3 <- '
        stipple3 (3-argument stipple) receives an index matrix
        mi (arg 1), a vector of values z (arg 2), and the
        dimensions .dim (arg 3) of an array to be created.  The
        return is an array of dimension .dim whose elements
        indexed by mi are populated with the corresponding
        values of z.

        mi <- matrix(c(1, 2, 1, 2), 2)

        mi

        #      [,1] [,2]

        # [1,]    1    1

        # [2,]    2    2
        
        z <- c(1, 2)

        stipple3(mi, z, c(2, 2))


        # [,1] [,2]

        # [1,]    1   NA

        # [2,]   NA    2
        '



swap.values <- function(X, a, b) {
	i <- if (a %|% is.na) X %|% is.na else X == a
	j <- if (b %|% is.na) X %|% is.na else X == b
	X[i] <- b
	X[j] <- a
	X }

    Doc$swap.values <- '
        swap.values returns a modified copy of the vector or
        array argument.  The modfication is to swap the values
        of elements having the values given by arg 2 and arg 3.

        swap.values(1:6, 3, 4)

        [1] 1 2 4 3 5 6'

ndim <- function (a)
        max(1L, a %|% dim %|% `#`)

    Doc$ndim <- '
        ndim returns the number of dimensions in the vector or
        array argument.'

vectorize.step <- function(a) {
	if (a %|% is.vector) return (a)
	if (a %|% ndim == 1L) return (a)
	DIM <- dim(a)
	dim(a) <- DIM[1:2] %|% prod %,% DIM[-2:-1]
	a }

	Doc$vectorize.step <- '
		vectorize.step returns a modified copy of the
		array argument, where the dimension has been
		lowered one step and the lower two dimensions
		of the argument are represented in the first
		dimension of the return.

		vectorize.step(array(1:8, c(2,2,2)))

		     [,1] [,2]

		[1,]    1    5

		[2,]    2    6

		[3,]    3    7

		[4,]    4    8'

mi.a <- function (a)
        arrayInd(a %|% as.logical %|% which, a %|% dim)

    Doc$mi.a <- '
        mi.a returns an index matrix of all the TRUE values in
        the array argument, which is interpreted as a logical
        array internally.'

idrop <- function(i)
        if (i %|% `#` == 1 && i == 1) i else i[which(i != 1)]

    Doc$idrop <- '
        idrop returns a modified copy of the vector argument
        with all one-values removed.
        
        Intended use: developing dimensions for simplified
        arrays.
        
        cf. base::drop.'

multiple.extract <- function (
    X, i, XEXTRACT=`[[`, IEXTRACT=`[[`, XXEXTRACT=`[[`)
        lapply(
            X %|% seq_along,
                function(k)
                    XXEXTRACT(
                        XEXTRACT(X, k),
                        IEXTRACT(i, k) ) )

    Doc$multiple.extract <- '
        multiple.extract takes a container X and an index i, which are considered parallel.
        The corresponding items of the container and the index are applied to element extraction.
        The optional arguments may be used to specify how '

extract.parallel <- function(.list, mi)
		lapply(
			mi %|% colNos,
			function(k) k %th% .list %=>% `[` %:|% mi[,k])

    Doc$extract.parallel <- '
        extract.parallel performs `x[i]` for each item x in the
        .list (arg 1), where the i are the corresponding columns
        of the index matrix mi (arg 2).
        
        > extract.parallel(list(LETTERS, 10:35), cbind(1:6, 1:6))

        [[1]]

        [1] "A" "B" "C" "D" "E" "F"

        [[2]]

        [1] 10 11 12 13 14 15
        '

a.tabulate <- function (FUN, LIST) {
	DIM <- vapply(LIST, length, 1 %|% integer)
	mi <- DIM %|% ai.every
	X <- extract.parallel(LIST, mi) %|% data_frame
	dfapply(X, FUN) %=>% dimension %:|% DIM }

    Doc$a.tabulate <- '
        a.tabulate returns an array of values which are computed
        by applying FUN to every combination of values found in
        LIST.

        > a.tabulate(`*`, list(1:5, 1:5))

            [,1] [,2] [,3] [,4] [,5]

        [1,]    1    2    3    4    5

        [2,]    2    4    6    8   10

        [3,]    3    6    9   12   15

        [4,]    4    8   12   16   20

        [5,]    5   10   15   20   25
    '

iblock <- function() doc(iblock)

    Doc$iblock <- '
        an iblock is a 2-row integer matrix. Each column
        contains a range of indices (including possibly
        hypothetical, negative indices) along the corresponding
        dimension of a hypothetical array.'

'
page #DELETED (AVOID CONFLICT WITH utils::page). USE `page.`.
'

page.m <- function (a, n=1) #TAGS array 3D 3-D slice
        a[,,n]

    'page returns the n-th (arg 2) matrix of the 3-D array
    argument (arg 1).'

page. <- page.m #DEPRECATED USE page.m


a.dummy <- function (dim)
        array(
            rapply.(
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

a.stack.l <- placeholder

MAT1 <- matrix(1)

dimension <- function (x, .dim) {
    dim(x) <- .dim;   x }

complete.dim <- function (x, .dim) {
    .dim <- .dim %|% ifloor
    DD <- .dim %|% iprod
    LG <- x %|% `#`
    if (DD < LG) {
        .dim <- c(.dim, LG %\% DD)
        DD <- .dim %|% iprod }
    if (DD > LG) stop(
        "dims [" %//% unwords(.dim) %//% "] product [" %//% DD %//% "] exceeds length [" %//%
        LG %//% "] of object.")
    .dim }

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



function aapply : apply

        A wrapper for vapply. However, the return is an array of
        the same dimension as the primary argument. Therefore
        FUN must return a single value.

'
