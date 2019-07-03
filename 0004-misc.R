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


shift_euclid.pg <- `-` %|% m.FUN.m..v #TAGS translate geometry

shift_euclid.xy <- shift_euclid.pg #DEPRECATED use shift_euclid.pg

    Doc$shift_euclid.pg <- '
        shift_euclid.pg returns a modified copy of the given
        polygon (arg 1). The returned polygon represents a datum
        shift according to arg 2. Apply negation to arg 2 to
        accomplish and effective translation to the (geometric)
        "right."'

overlay <- function(...) #TAGS mask prefer combine or graphic image transparent transparency
        rapply(cbind(...), firstAvailable) 

overlay.m <- function(...) #TAGS mask prefer combine or graphic image transparent transparency
        dimension(
            overlay(parallelize(...) %|% t),
            dim(list(...)[[1]]))

    Doc$overlay <- '
        Given any number of aligned, atomic vectors, overlay
        returns an atomic vector where elements consist of the
        first available (i.e., non-NA) elements from among those
        found in the corresponding positions of the arguments.

        overlay.m behaves similarly, except the arguments may be
        arrays, and the return has the same dimensions as the
        primary argument. Expect errors or strange behavior if
        not all arguments are of the same length.
        
        HISTORY
        
                2018-07-25
                        changed form of input and output,
                        updated documentation.
                Earlier
                        previously returned first available
                        nonzero values.'
