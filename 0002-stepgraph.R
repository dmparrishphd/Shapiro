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
stepgraph <- function (X=NULL, Y=X, ...)
        linegraph(X=lapply(X, stepsx), Y=lapply(Y, stepsy), ...)

'
function stepgraph : plot

        A wrapper for linegraph. Takes same parameters. However,
        "stepped" versions of the vector members of the X and Y
        lists are converted passed to linegraph rather than the
        vectors themselves.  Example:

                stepgraph(list(0:9), list(0:8))

        produces a graph similar to the floor function.
'
