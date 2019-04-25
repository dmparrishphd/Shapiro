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

rasters=function( #TAGS plot image grid
        x, y, z, zlim=z %|% finites %|% range,
        col=12 %|% heat.colors,
        add=T,
        xaxs="i", yaxs="i",
        xlab="", ylab="",
        breaks=NULL, oldstyle=F, useRaster=T, ...)
    image(x, y, z,
        col=col, add=add,
        xaxs=xaxs, yaxs=yaxs,
        xlab=xlab, ylab=ylab,
        breaks=if(breaks %|% is.null) {
            image_breaks(col %|% `#`, zlim)
        } else {
            breaks},
        oldstyle=oldstyle, useRaster=useRaster, ...)
        
    Doc$rasters <- '
        rasters is to image as plot is to points. The rasters
        function is designed to plot a matrix onto an existing
        plot---in typical usage, xlim and ylim are left
        unspecified.'

