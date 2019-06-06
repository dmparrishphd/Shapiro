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


as.ordinal <- function (x, levels=sort(unique(x)))
        list(INDICES=match(x, levels), LEVELS=levels)

as.ordinal.a <- function (a, levels=sort(unique.a(a)))
        list(
            INDICES=dimension(
                as.ordinal(a %|% as.vector, levels=levels)$INDICES,
                a %|% dim),
            LEVELS=levels)

l.readTIFF <- function (...) {
    TIF <- readTIFF(...)
    if (TIF %|% is.matrix) return (dimension(TIF, dim(TIF) %,% 1) %|% list)
    if (TIF %|% is.array ) return (TIF %|% list)
    if (TIF %|% first %|% is.matrix) return(lapply(TIF, function(m) dimension(m, dim(m) %,% 1)))
    TIF }

    Doc$l.readTIFF <- '
        l.readTIFF is a wrapper for readTIFF (library tiff). All
        arguments are passed to readTIFF. The return is always a
        list of 3-D arrays, one page per channel.'


bin.itiff <- function (infile, outfile) {
    MAP <- lapply(
        1:CHANNELS,
        ) 



     }

    Doc$tiff2bin <- '
        tiff2bin extracts image data from a TIFF file and writes
        it to a group of files that represent the TIFF data in a
        more accessible format.'
