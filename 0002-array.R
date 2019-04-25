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

pbind <- function (mm) {
    x <- c(mm, recursive=T)
    dims <- dim_complete(x %|% `#`, mm[[1]] %|% dim)
    y <- vector(mode=x %|% typeof, length=dims %|% prod)
    y[1:(x %|% `#`)] <- x
    dimension(y, dims) }

    Doc$pbind <- '
        pbind is similar to cbind or rbind; pbind is designed to
        combine matrices into a 3D array.
        
        The argument is a list of arrays and/or vectors. The
        first item in the list determines the lower dimensions
        of the return, and the highest dimension of the return
        is large enough so that the return may contain all of
        the values found at the leaves of the list argument
        (internally the recursive argument of `c` is set to
        TRUE).

        Any elements in the return that are present merely to
        accomodate the dimension have the default value
        corresponding to the type of the return (i.e., FALSE,
        0L, 0, 0+0i, "")
        
        Example:
        
        > M <- matrix(1:4, ncol=2)

        > M

        #        [,1] [,2]

        #   [1,]    1    3

        #   [2,]    2    4

        > pbind(list(M)) # NOTE THE DIMENSION OF THE RETURN

        #   , , 1

        #        [,1] [,2]

        #   [1,]    1    3

        #   [2,]    2    4

        > pbind(list(M, M))

        #   , , 1

        #        [,1] [,2]

        #   [1,]    1    3

        #   [2,]    2    4

        #   , , 2

        #        [,1] [,2]

        #   [1,]    1    3

        #   [2,]    2    4

        > pbind(list(dimension("AS  YOU WISH", 1), TRUE, "LOVE")) %|% t

             [,1]
        [1,] "AS  YOU WISH"
        [2,] "TRUE"
        [3,] "LOVE"
        
        '
