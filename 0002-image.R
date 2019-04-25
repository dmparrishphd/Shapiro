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

.image_breaks.n <- seq0 %O% (`+` %<=% .5)

.image_breaks.h <- `#` %O% image_breaks.n

.image_breaks.n..d <- function(ncolors, rangez)
        seq(from=rangez[1], to=rangez[2], length.out=1L + ncolors)

image_breaks <- function (x, rangez=NULL) {
    (#DISPATCH ON TYPE
     if (rangez %|% is.null) {
        if (x %|% mode == "numeric") {
            if (x %|% `#` < 1) {
                nop
            } else if (x %|% `#` < 2) {
                .image_breaks.n
            } else {
                x <- x %|% `#`
                .image_breaks.n }
        } else if (x %|% typeof == "character") {
            .image_breaks.h }
    } else {
        function(n) .image_breaks.n..d(n, rangez)
    })(x)
}

    Doc$image_breaks <- '
        image_breaks determines the breaks parameter for image
        in a manner consistent with the default (i.e., evenly
        spaced intervals over the number of colors).  Arguments
        may take one of three forms:
        
        1) Two parameters, where the first is an integer
        specifying the number of colors and the second is the
        range of values;
        
        2) A single numeric parameter specifying the number of
        colors;
        
        3) A character vector color palette, or

        4) A numeric vector color palette
        
        Under forms 2, 3, and 4 the z-values are assumed to be
        natural numbers up to and including the number of
        colors.'

