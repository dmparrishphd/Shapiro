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



project <- function(projection, points)
	capply(points, projection, points %|% nrow %|% double)

    Doc$project <- '
        project returns a point matrix of projected points.
    
        Arg 1, projection, is a map projection---a function of
        an n-dimensional point that returns an n-dimensional
        point---and
        
        arg 2 is a matrix of points (one column per point). 

        The projection argument might be curried from another
        function that has not only a point as its argument, but
        several constant parameters as well, such as central
        meridian, false easting, etc.'

morph.m <- function (mtransformation, mpoints)
    vapply(
        mpoints %|% rowNos,
        function(i) mtransformation %*% t(rows(mpoints, i)),
        mpoints %|% ncol %|% double)

    Doc$morph.m <- '
        morph.m is intended to perform a transformation,
        described by the primary argument (a numeric, square
        matrix), on the points represented by the secondary
        argument (a numeric matrix having the same number of
        columns as the primary argument)
        
        Referenec: https://en.wikipedia.org/wiki/Transformation_matrix#Other_kinds_of_transformations
        Note: the information behind morph.m is believed to be in the category
        of common knowledge / public domain.'

`4x4` <- matrix %^% list(nrow=4, ncol=4) #TAGS transformation matrix geometry afine linear
`4x4 t` <- `4x4` %O% t #TAGS transformation matrix geometry afine linear

    Doc$`4x4 t` <- '
        `4x4 t` returns a four-by-four matrix whose elements are
        taken from the 16-element vector argument.

        This function is designed for use in source code, where
        the values can be coded in a manner that is visually
        consistent with the return; i.e., "down" is the major
        (along column) and "right" is the minor (along row)
        direction.
        
        The original intent of `4x4 t` for use in developing
        transformaiton matrices for common geometric afine
        transformations. They upper 2x2 or 3x3 portion may be
        used for linear transformations.'

MORPH.MIRROR.XZ <- #TAGS transformation reflection x xz
   '     1   0   0   0
         0  -1   0   0
         0   0   1   0
         0   0   0   0  ' %|% i.h %|% `4x4 t`

MORPH.MIRROR.YZ <- #TAGS transformation reflection x xz
   '    -1   0   0   0
         0   1   0   0
         0   0   1   0
         0   0   0   0  ' %|% i.h %|% `4x4 t`

mirror.xz <- morph.m %<=% MORPH.MIRROR.XZ

mirrorx.xy <- (morph.m %<=% MORPH.MIRROR.XZ[1:2, 1:2]) %O% t
mirrory.xy <- (morph.m %<=% MORPH.MIRROR.YZ[1:2, 1:2]) %O% t

