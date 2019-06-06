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


i.look.d <- function(x, table) #TAGS search interpolate
        if (x <= table %|% first || table %|% last < x) {
            iNA %|% rep2
        } else {
            which1(x <= table) + .lo }

    Doc$i.look.d <- '
        i.look.d is intended to find values in table, a sorted
        atomic vector of unique numeric values. Returns the
        first **** TWO INDICES **** k + 0:1, table[[k]] < x <=
        table[[k+1]]. The choice of ~ < x <= ~ rather than ~ <=
        x < ~ is intended to fit indexing from 1 and the notion
        that, when plotting a matrix as a raster, the local
        coords 0:1 are associated with the first element.
        Suggestion: pad a vector with -Inf and Inf to guarantee
        finite return for a finite primary argument.
        
        > tbl <- c(-Inf, 0, 1, Inf)

        > i.look.d(0.5, tbl)

        [1] 2 3

        > i.look.d(1, tbl)

        [1] 2 3

        > i.look.d(1.01, tbl)

        [1] 3 4'

