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


