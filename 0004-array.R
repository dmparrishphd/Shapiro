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
dimensions.iranges <- function (iranges) capply(iranges, dimension.irange)

block.extract.a <- function(a, iranges) {
    warning("PLACEHOLDER"); return (NULL)
    # modify the ranges
    lo <- capply(rbind(    1L, row.m(iranges, 1)), max)
    hi <- capply(rbind(dim(a), row.m(iranges, 2)), min)
    aa <- array(dim=dimensions.iranges(iranges))
    aa
}

'
function dimensions.iranges : array indices

        Multidimensional version of dimension.irange. The
        argument matrix has one column per dimension, with the
        "fastest" dimension first.
'

