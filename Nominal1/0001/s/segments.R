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

pairs.xy <- function (xy) {
    if (xy %|% `#` %% 4) xy <- rbind(xy, NA)
    pp <- xy %|% `#` / 4
    dimension(xy %|% t, 4 %,% pp) %|% t }

segments.m <- function(x, ...)
	    segments(x[,1], x[,2], x[,3], x[,4], ...)

    Doc$segments.m <- '
        segments.m is a wrapper for segments. The first four
        columns of the matrix or data frame argument are passed
        to segments as x0, y0, x1, and y1, respectively. ... is
        also passed.'

segments.xy <- function (xy, ...)
        segments.m(xy %|% pairs.xy, ...)

segmentsv.m <- function(x, ...)
	    segments(x[,1], x[,2], x[,1], x[,3], ...)

    Doc$segmentsv.m <- '
        segmentsv.m is designed for plotting possibly disjoint,
        vertical line segments passes columns 1, 2, and 3 of the
        matrix or data frame argument to the arguments of
        segments: column 1 is passed to the x0 and x1 args,
        column 2 is passed to the y0 arg, and column 3  is
        passed to the y1 arg. ...  is also passed.'


