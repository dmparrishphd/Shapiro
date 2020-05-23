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

#FUTURE: MOVE TO LOWER LEVEL
unpopulated <- is.na %O% `!` %O% any %O% `!` #TAGS empty missing NA

b.unpopulated.cols <- function(X) #TAGS empty missing NA
        capply(X, unpopulated)

b.populated.rows <- function(X) #TAGS nullable populated complete record select filter which NA
        capply(X %|% is_populated %|% t, all)

    Doc$b.populated.rows <- '
        b.populated.rows returns a logical vector indicating
        which of the rows of the matrix or data frame argument
        are completely populated (i.e., contain no NA values).'

populated.rows <- function(df1) #TAGS nullable populated complete record select filter which NA
        df1[df1 %|% b.populated.rows, , drop=F]

    Doc$populated.rows <- '
        populated.rows returns the rows of the matrix or data
        frame argument which have no NA values.'

