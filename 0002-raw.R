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

bits.b <- pad %|% argswap %<=% 8 %O% as.raw %O% packBits

	Doc$bits.b <- '
        bits.b returns a raw vector that represents the logical
        vector argument.
        
        > bits.b(rep(T,10))

        [1] ff 03'

b.bits <- rawToBits %O% as.logical

	Doc$b.bits <- '
        b.bits returns a raw vector that represents the logical
        vector argument.
        
        > bits.b(rep(T,10))

        [1] ff 03'

shl <- rawShift %|% argswap %<=%  1L
shr <- rawShift %|% argswap %<=% -1L

lsb.n <- `%%` %|% argswap %<=% 256L

lsb <- floor %O% as.integer %O% lsb.n

    Doc$lsb <- '
        lsb and lsb.n return the integer values corresponding
        the the least significant byte of the unsigned
        integer-valued vector. lsb.n may be used for integer
        vector arguments.'

    Doc$lsb.n <- Doc$lsb

u.r <- function(r)




