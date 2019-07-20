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
	for (k in tychs %|% seq_along)
		Return[i] <- replace.a.at(Return, tychs[[k]], at[[k]]) %|% as.vector
	Return }

        Doc$polyptych <- '
            polyptych returns an array of dimension .dim (arg 1)
            whose elements are determined by tychs (arg2, a list
            of arrays of the same number of dimensions as that
            specified by .dim) and at (arg 3), which specifies,
            for each item in tychs, a location of the same
            number of dimensions as that specified by .dim.
            Each member of at specifies the location of the
            first cell of the corresponding element of tychs.
            
            SUGGESTION. Use together with overlay if a default
            value other than NA is desired.

            see also diptych.

            polyptych(
                8 %|% rep2,
                list(matrix(42), matrix(43, nrow=2)),
                list(c(1,1), c(7,8)))
            '
