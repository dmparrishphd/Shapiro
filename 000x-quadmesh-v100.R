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

list(
    #TODO: include provision for different orientations?
    #This version: trying to include only the essential qualities.
    DOC=list(
        self='
            works with an attribute array that stores the
            cartesian cooridinates of points indexed by
            their topological coordinates in a quad mesh.

            See also 0001-attrib-array--proto.R',

        `.i.[[`='
            returns a matrix of topological indices of the
            points that form the quadrilateral of the cell
            whose n-dimensional index is given.',

        `[[`='
            returns the points that form the quadrilateral of
            the cell whose n-dimensional index is given.',

        `in`='
            returns a vector indicating, for the quad mesh cell
            indicated by the n-dimensional index (arg 2),
            whether each of the points (arg 3) is in the cell.
            Arg 3 will be converted to a matrix of points (one
            point per column) by the default method.
            
            HISTORY: formerly (2019-06-16) used area coords for
            itnternal calcs for single points.',

        .NULL='' ),

    init=function(., points.) {
        .$.POINTS <- points.
        .$.DIM <- .$.POINTS$.DIM - 1L
        . },
    dim=function(.) .$.DIM,
    `.i.[[`=function(., i) xy.square.() %m+v% i,
    `[[`=function(., i) .$.POINTS$`[`(.$.POINTS, .$`.i.[[`(., i)),
    `in`=function(., i, points) { #ASSUMPTION: QUAD IS CONVEX
        i <- sign_coords(.$`[[`(., i), points %|% matrix)
        !capply(i, 0L %=>% `>` %O% any, T) & # NOT OUTSIDE
        i[2,] & # NOT ON LINE 2. REMINDER: 0L ==> ON THE LINE
        i[3,]   # NOT ON LINE 3
        },
    .NULL=NULL )
