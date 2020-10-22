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



m.lines.pg <- pairwise %|% argswap %<=% line.pp

    Doc$m.lines.pg <- '
        m.lines.pg returns a matrix of lines, one for each pair
        of points in the polygon argument, one line per column
        of the return. The order of the lines is consistent with
        the order of the points of the polygon argument (i.e.,
        the first line is from point 1 to point 2)
        
        HISTORY
        
        renamed to m.lines.pg from lines_pg to avoid name
        conflict with lines_pg, which plots a polygon.'


quadrance.m <- diff %O% ssq

    Doc$quadrance.m <- '
        quadrance.m returns the quadrance between the two points
        represented by their Cartesian coordinates, found in the
        rows of the 2xn matrix argument.'

quadrance <- function (x1, x2)
        rbind(x1, x2) %|% quadrance.m

quadrance.l <- function(X)
        list() %,% 0L %=:% "pad" %v% m.l %-|% X %|% t %|% quadrance.m

    Doc$quadrance.l <- '
        quadrance.l returns the quadrance between the two points
        represented by their Cartesian coordinates, found in the
        elements (numeric vectors) of the 2-element list argument.'



