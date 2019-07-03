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


rapply  <- function (m, FUN, FUN.VALUE=NULL, ...)
        vapply(
            m %|% rowNos,
            function (i, m, ...) FUN(m[i,], ...),
            if (FUN.VALUE %|% is.null) FUN(m[1,], ...) else FUN.VALUE,
            m,
            ...)

    Doc$rapply <- '
        rapply is similar to capply, but operates on rows of the
        primary, matrix argument, rather than columns'

