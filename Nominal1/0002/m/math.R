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

floor.ceiling <- floor %,% ceiling %|% multifunction

i.full.range <- full.range %O% floor.ceiling %O% range

quotient <- function (x) {
    L <- x %|% `#`
    if (L < 1) {
        x %|% typeof %|% na
    } else if (L < 2) {
        x
    } else if (L < 3) {
        x[1] / x[2]
    } else {
        x %|% typeof %|% na } }

    Doc$quotient <- '
        quotient returns the quotient of the elements of a
        two-vector. If the argument is a single value, this
        value is returned (i.e., the denominator is assumed to
        be 1). For vectors of length 0 or greater than 2, NA (of
        the same type as the argument) is returned.'

