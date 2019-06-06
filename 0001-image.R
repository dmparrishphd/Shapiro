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


image_x.n <- seq0
image_y.n <- seq0

    Doc$image_x.n <- '
        image_x.n and image_y.n are aliases for seq0. Intended
        use: given the number of pixels in the x or y direction
        of an image, returns the natural "x" or "y" argument for
        image.'

    Doc$image_y.n <- Doc$image_x.n

image_x.m <- nrow %O% seq0
image_y.m <- ncol %O% seq0

    Doc$image_x.m <- '
        image_x.m and image_y.m are similar to image_x.n and
        image_y.n, except that the argument is the matrix to be
        displayed rather than the number of pixels.'
