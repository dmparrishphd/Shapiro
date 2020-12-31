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

rect.xy <- function(xy, ...) {
    S <- xy %|% nrow %/% 2L %|% seq * 2L
    rect(
        xy[S - 1L, 1L],
        xy[S - 1L, 2L],
        xy[S     , 1L],
        xy[S     , 2L], ...)
}

    Doc$rect.xy <- '
        rect_xy is a wrapper around rect. The coords are specified with
    an xy matrix rather than with four vectors.'


xy.regular.polygon.xy <- function (n) {
    a <- seq(from=-pi, to=pi, length.out=1+n)
    cbind(cos(a), sin(a)) }

'
function xy.regular.polygon.xy : lines

        Returns an xy matrix representing the vertices of a
        regular polygon (with some rounding error). The last row
        of the matrix is the same as the first (the first and
        last points are duplicates). The return could be used to
        plot the polygon using the lines or polygon functions.

'
