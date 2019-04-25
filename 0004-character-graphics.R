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
glyph.encode.h <- function (h, ncol=8)
        sum.m(2 ** c(1:ncol - 1L) *
                matrix(h % % dec.sift.h % % as.integer, ncol=ncol))

'
function glyph.encode.h : character graphics binary

        Given a character string of 0 and 1 characters
        (whitespace ignored) that may represent positive and
        negative space in an image (such as are used to produce
        monochrome characters on a computer screen), returns an
        integer vector whose values represent, bit-wise, the
        given character string. Exmaple:

                > glyph.encode.h("
                + 01111100
                + 01100110
                + 01100110
                + 01111100
                + 01111000
                + 01101100
                + 01100110
                + 00000000")
                [1]  62 102 102  62  30  54 102   0
                
'
