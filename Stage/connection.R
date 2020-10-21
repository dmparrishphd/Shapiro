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


file.read.only <- "r" %=:% "open" %v% file

file.read.only.binary <- "rb" %=:% "open" %v% file

'
endianness.default = "little"

#seek.r = function(con, ...) {  }

lc.filerb = latent.call(file, list(open="rb"))

seekrec = function (con, rec, recsize) seek(con, (rec-1)*recsize)

lc.readBin = latent.call(readBin, list(endian=endianness.default))

fnreadBinData = function (what=integer(), n = 1L, size = NA_integer_, signed = TRUE, endian = .Platform$endian)
{   function(con) readBin(con, what, n=n, size=size, signed=signed, endian=endian)
}

'
