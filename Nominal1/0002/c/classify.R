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

breaks.quasi.log <- function(decades=1)
	(matrix(c(1, 2, 5), nrow=decades, ncol=3, byrow=T) *
	crep(10 ^ seq0(decades - 1), 3)) %|% t

    Doc$breaks.quasi.log <- '
        breaks.quasi.log returns a sequence of values on a
        quasi-logarithmic (base 10) scale. The argument
        specifies the number of decades.

        TIPS

        If the NUMBER OF BINS desired is not a multiple of 3,
        use, e.g., `[` to remove unneeded elements from a return
        that has more than enough bins.

        The return may be scaled to produce the desired range of breaks.

        > breaks.quasi.log()

        [1] 1 2 5

        > breaks.quasi.log(3)

        [1]   1   2   5  10  20  50 100 200 500

        > 100 * breaks.quasi.log(3)

        [1]   100   200   500  1000  2000  5000 10000 20000 50000
        '

