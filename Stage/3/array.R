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



polyptych <- function(.dim, tychs=list(), at=list()) { #TAGS combine replace array diptych triptych
	Return <- array(tychs %|% promotion + NA, .dim)
	i <- .dim %|% prod %|% seq
	for (k in vapply2(list(tychs, at), `#`) %|% min %|% seqN)
		Return[i] <- replace.a.at(Return, tychs[[k]], at[[k]]) %|% as.vector
	Return }

        Doc$polyptych <- '
            polyptych returns a single array from a list of arrays.

            ARGUMENTS

            1.  .dim specifies the dimension of the return.

            2.  tychs is a list of arrays. The number of
                dimensions of each array is length(.dim).

            3.  `at` specifies, for each item in tychs, the
                location (first cell) in the return to which the
                corresponding array is copied.

            THE NUMBER OF TYCHS used is determined by the
            shorter of the lengths of tychs and `at`.

            SUGGESTION. Use together with overlay if a default
            value other than NA is desired.

            see also diptych.

            polyptych(
                8 %|% rep2,
                list(matrix(42), matrix(43, nrow=2)),
                list(c(1,1), c(7,8)))
            '

fn.polyptych.regular <- function(.dim.outer, .dim.inner)
		lapply(
			lapply(
				.dim.outer %|% seq_along,
				function(k) seq0(length.out=.dim.outer[k]) * .dim.inner[k]),
			succ) %|%
		grid_regular.aligned %|% t %|% l.unstack.a %|%
		list %=:% "at" %v% polyptych %<=% (.dim.outer * .dim.inner)

    Doc$fn.polyptych.regular <- '
        fn.polyptych.regular returns a one-argument, curried
        version of polyptych that, in turn, produces a regular
        polyptych---regular in the sense that the positions of
        the constituent tychs ("folds") form a regular grid.

        The argument of the returned function is the list of
        arrays to be formed into a polyptych.

        ARGUMENTS

        1.  .dim.outer is the dimensions of the polyptych in number of tychs.

        2.  .dim.inner is the dimensions of the constituent tychs.

        fn.polyptych.regular(rep2(2), rep2(4))(lapply(1:4, array %|% argswap %<=% rep2(1)))

    '

            
