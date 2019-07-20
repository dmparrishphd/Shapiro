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


page_setup <- function( #TAGS windows x11 dev device image plot
        x=0:1, y=0:1, xlab="", ylab="", xaxt="n", yaxt="n",
        bty="n", ...) {
    image(
        x=x, y=y, z=matrix(),
        zlim=c(0, 1), # AVOIDS WARNING
        xlab=xlab, ylab=ylab,
        xaxt=xaxt, yaxt=yaxt, bty=bty,
        ...) }

    Doc$page_setup <- '
        page_setup clears the current device and sets the user
        coordinates (x and y). All other arguments are passed to
        image. The defaults ("none") are designed for
        incremental drawing.
        
        DETAILS
        
        PDF. REMINDER: image will createa new page unless add=T.
        Typicall, the next call to image after page_setup will
        have add=T. Similarly for plot? lines? points?.'

