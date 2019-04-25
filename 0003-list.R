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
t.list <- function(l)
		if (l  % %  `#`  % %  `!` ||
				l[[1]]  % %  is.list  % %  `!`) l else
		rename.all(
			lapply(
				seq_along(l[[1]]),
				argswap(Curry(argswap(lapply), `[[`)),
				l),
			names(l[[1]]))

        Doc$t.list <- "
                The generic function t (transpose) is overloaded
                for a list argument. For empty lists, returns
                the argument. For linear lists (AS JUDGED BY THE
                FIRST ELEMENT), returns its argument. For
                compound lists (AS JUDGED BY THE FIRST ELEMENT),
                the first element of the outer list sets the
                assumed pattern (length and names) for the
                remainder of the outer list; returns the
                transposed compound list."

        warning("Defining t.list for generic function t.")

df.l <- function (l)
        lapply(l  % %  t, unlist)  % % data.frame


'
function t.list

        See also Doc$t.list

        
funciton df.l

        Example:

                > df.l(list(list(A=1,B=2,C=3), as.list(4:6), as.list(7:9)))
                  A B C
                1 1 2 3
                2 4 5 6
                3 7 8 9

        
'
