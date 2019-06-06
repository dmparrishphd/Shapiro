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

multiples.near <- placeholder

    Doc$multiples.near <- '
        (PLACEHOLDER) multiples.near returns the TWO multiples of arg 1 that
        are nearest of arg 2. Intended for integer-valued inputs.'

d.r <- function(r) sum(r %|% as.integer * 256^seq0(r %|% `#` %|% pred))

    'Doc$d.r <-
        d.r returns a double value formed from the base-256
        unsigned integer represented by the raw vector argument.
        The return may not be accurate for arguments longer than
        6 elements (48 bits).'

idiv <- function(i, n) i %|% pred %/% n %|% succ #TAGS index division divide quotient
imod <- function(i, n) i %|% pred %%  n %|% succ #TAGS recycle index remainder

    Doc$idiv <- '
        idiv (indexing division) performs integer division,
        rounding up.'

	Doc$imod <- '
		imod ("indexing" mod) is similar to `%%`. However, like the clock, the
        base is 1 rather than 0. Therefore
        
        imod(12L, 12L) == 12L # whereas
        12L %% 12L == 0L

        and

        all(vapply_(1:(12*n), imod %|% argswap %<=% 12L) == rep(1:12, n))

        where n is a natural number.'


