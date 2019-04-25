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
diptych <- function(
        z1,
        z2,
        x=0:(z1  % %  nrow + z2  % %  nrow),
        y=-ncol(z1):0, ...)
    image(x, y, rbind(z1, z2), ...)

'
function diptych : image side-by-side

        Wrapper for image. Note the order of the first four
        parameters is different from image.

        Display image matrices side-by-side on the current
        device.
'

