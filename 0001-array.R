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



arrayInd.corners. <- function (.dim)
        rapply(
           .dim %|% `#` %|% rep2 %|% arrayInd.every,
           .dim %|% arrayInd.range %=>% `%[ZZt%`,
           .dim %|% `#` %|% integer) %|% t

    Doc$arrayInd.corners. <- '
        arrayInd.corners. is similar to arrayInd.corners, except
        for the **** ASSUMPTION **** that the dimensions are at
        least 2 in every direction. If this assumption does not
        hold, the return will contain duplicates.'

arrayInd.corners <- placeholder

    Doc$arrayInd.corners <- '
        arrayInd.corners (FUTURE) returns the indices of the
        corners of an array or vector.'

arrayInd1 <- arrayInd %<=% 1L

array.spec.contiguous <- function (li)
        lapply(li, function(x) swmatch %<=% x)

    Doc$array.spec.contiguous <- '
        array.spec.contiguous is designed to return a list of
        functions that may be used for array indexing:

        The argument is a vector or list of values.

        The return is a list of functions that each map the
        values of the corresponding argument element to the
        first few natural numbers.

        see also *iarray--proto--test.R

        > specs <- array.spec.contiguous(
            list(
                c("ZERO", "ONE", "TWO"),
                c(F, T, NA)))

        > specs[[2]](NA)

        [1] 3

        > specs[[2]](T)

        [1] 2

        > specs[[2]](T)> specs[[1]]("ZERO")

        [1] 1'

#APPEND ONLY! DESIGNED FOR CHARACTER ARRAYS ONLY!
write.a <- function(x, file=stdout(), ncolumns=1L) {
	N <- (if (x %|% is.array) nrow else `#`)(x)
	for (i in x %|% `#` %/% N %|% seq - 1L)
		write(
			x[N %|% seq + N * i],
			file=file, ncolumns=ncolumns, append=T, sep="") }

    Doc$write.a <- '
        write.a **** APPENDS **** the array argument (arg1) to
    the file (arg2) specified. No more than ncolumns elements
    are written to each line of the text output. Blocks of text
    representing columns of the array will be separated by
    newline characters.

    write.a(letters, ncolumns=5, file("filename", open="w"))

    will result in the file filename being created / overwritten
    with the contents:

    abcde*fghij*klmno*pqrst*uvwxy*z*

    where * is an end-of-line sequence

    see also write.table.bare.
    '

offset. <- function(X, shift=0L)
	lapply(
        if (X %|% is.list) X %|% seq_along else 1L,
		function(i) shift %[mod% i + enlist(X)[[i]])

    Doc$offset. <- '
        offset. returns a list containing modified versions of
        arg1, which is a (list of) numeric vector(s).  The
        modification is to add (each of) the value(s) specified
        in the shift vector to the corresponding vector element
        of arg1. The shifts are recycled to obtain an effective
        shift vector that is the same length as arg1. Originally
        intended to compute global array indices given local
        indices and an offset.'

replace.a.at <- function (a, b, where) {
    if (a %|% dim %|% is.null) dim(a) <- a %|% `#`
    if (b %|% dim %|% is.null) dim(b) <- b %|% `#`
    INCOMPATIBLE.DIMENSIONS <- a %|% ndim != b %|% ndim
    if (INCOMPATIBLE.DIMENSIONS)
            'Number of dimensions of primary and secondary
            arguments are incompatible.' %|% crunch.h %|%
            warning
    if (
        INCOMPATIBLE.DIMENSIONS |
        b %|% is.empty | # b HAS NO ELMENTS
        any(a %|% dim < where) # a DOES NOT EXTEND TO where
    ) {
        a 
    } else {
        replace.a(
            a,
            list.=lapply(
                where %|% seq_along,
                function(i)
                        where[i]:min(
                            dim(a)[i],
                                    # LIMIT BY DIMENSIONS OF a
                            where[i] + dim(b)[i] - 1L
                                    # LIMIT BY DIMENSIONS OF b
                        ) ),
            values=extract.a(
                b,
                lapply(
                    b %|% dim %|% seq_along,
                    function(i)
                        min(
                            dim(a)[i] - where[i] + 1L,
                                    # LIMIT BY DIMENSIONS OF a
                            dim(b)[i]
                                    # LIMIT BY DIMENSIONS OF b
                        ) %|% seq)
                    ) ) } }

dimension.irange <- function (irange) #TAGS vector index indices
        1L + irange[2] - irange[1]

    Doc$dimension.irange <- '
        Given an integer range, c(m, n), m <= n, returns the
        number of elements in a corresponding hypothetical
        vector.'

as_array_of <- function(a, mode="logical")
        dimension(as.vector(a, mode=mode), a  % %  dim)

'
function as_array_of : convert

        Similar to as.vector, but the result has the same
        dimensions as the primary argument

'

