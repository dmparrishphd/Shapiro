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

padding <- function(x, n) {
    len <- x %|% `#`
    vector(
        mode=x %|% typeof,
        length=ceiling_multiple(n, len) - len) }

pad <- function (x, n=2)
        x %,% padding(x, n)

    Doc$pad <- '
        pad returns a possibly longer copy of the primary
        argument vector or list. The length of the return is a
        multiple of n (arg 2).  The values of any additional
        elements are determined by the default for the vector
        function.'

