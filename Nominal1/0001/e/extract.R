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

inner <- rest %O% except.last

    Doc$inner <- '
        inner returns a modified copy of the vector argument
        without the first and last elements.'

irotate <- function (n)
        if (n < 2L) n else 2:n %,% 1L

    Doc$irotate <- '
        irotate returns the indices of a rotated vector of
        length n. The hypothetical rotation occurs by placing
        the first item at the end.'

rotate <- function (x)
        x[x %|% `#` %|% irotate]

    Doc$rotate <- '
        rotate returns a modified copy of the vector argument.
        The return is effectively formed by making a copy of the
        argument, then moving the first element to the end.'

rotater <- function (m)
        m[m %|% nrow %|% irotate,]

    Doc$rotater <- '
        rotater returns a modified copy of the matrix argument.
        The return is effectively formed by making a copy of the
        argument, then moving the first row to the bottom.'

rotatec <- function (m)
        m[,m %|% ncol %|% irotate]

    Doc$rotatec <- '
        rotatec returns a modified copy of the matrix argument.
        The return is effectively formed by making a copy of the
        argument, then moving the first column to the right.'

irotate..n <- function (L, n) {
    if (L  < 1L) return (0L)
    if (L == 1L) return (1L)
    if (n <= 1L) return (L %|% seqN)
    n <- imod(n, L) # GUARANTEES n <= L
    n:L %,% except.last(n %|% seqN) }

rotate..n <- function(x, n)
        x[irotate..n(x %|% `#`, n)]

	Doc$rotate..n <- '
		rotate returns a modified copy of the vector argument.
		The elements are in a different order, beginning with
		element n, progressing through to the last, then
		beginning again at element 1 through n - 1'

nibble <- function (X, n=1L, base=2L)
        col.m(
            matrix(
                X[base ^ decade(X %|% `#`, base) %|% seq],
                ncol=base),
            base %|% seq %[mod% n)

    Doc$nibble <- '
        nibble returns the n-th (arg2) "nibble" of the vector
        argument (arg1), according to the specified base (arg3).
        A vector of a given base is conceived as being comprised
        of base contiguous nibbles. For an 8-element vector x of
        base 2, nibble 1 is x[1:4] and nibble 2 is x[5:8]. For
        the purposes of the present function, the primary
        argument is conceived as being padded with zero or more
        NA values, up to a length equal to the smallest integer
        power of base that whose value is greater than or equal
        to the length of the primary argument. E.g., nibble(1:3,
        n=2, base=2) == c(3L, NA) and  nibble(1:14, n=4, base=4)
        == c(13, 14, NA, NA). In the first case, the primary
        argument is internally promoted to a vector of length 2
        ^ 2; in the second case the promotion is to a vector of
        length 4 ^ 2.'

i.nibble <- function (X, n=1L, base=2L)
        nibble(X %|% seq_along, n=n, base=base)

    Doc$i.nibble <- '
        i.nibble behaves much the same as nibble, except that it
        is the indices that are returned rather than the values.'

`]]` <- `[[` %|% argswap #TAGS extract
`-$` <- `$`  %|% argswap #TAGS extract

.first2 <- function(x) x[1:2]

    Doc$.first2 <- '
        .first2 is intended as an internal function called by
        first2.'

.first2.l <- function(X) {
	result <- NULLs(2)

	x <- first(X)
	if (x %|% is_empty %|% `!`) result[[1]] <- x

	x <- second(X)
	if (x %|% is_empty %|% `!`) result[[2]] <- x

	result }

    Doc$.first2 <- '
        .first2.l is intended as an internal function called by
        first2.'

first2 <- function(X) #TAGS extract
		if (X %|% is.vector)
                jump.b(
                    X %|% is.list,
                    .first2 %,% .first2.l,
                    X)

    Doc$first2 <- '
        first2 returns the first two elements of the list or
        vector argument. If the argument does not have two
        elements, the corresponding elements of the return are
        NULL (if the argument is a list) or NA (if the argument
        is an atomic vector).'



