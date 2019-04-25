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


par.usr.set.indirect <- function (scale.xy=ones(2), offset.xy=zeroes(2), new_page=F) #TAGS map scale window zoom domain
    image_bare(
        offset.xy[1] %,% (scale.xy[1] * par.uin.width()),
        offset.xy[2] %,% (scale.xy[2] * par.uin.height()),
        M1, col=TRANSPARENT, add=!new_page)

        Doc$par.usr.set.indirect <- crunch.h('
            par.usr.set.indirect sets the usr coords of the
            current device via scale.xy (user units per inch)
            and offset.xy (user units). Both arguments are
            optional, with the defaults resulting in usr coords
            matching the page. Originally intended to be called
            early in the process of creating a final image.')

