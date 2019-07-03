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



# Attribute Array
# Usage: source.object("current filename")
list(
    DOC='
        This object stores an atomic vector for
        each element of an array.

        [[ extracts the atomic vector associated with the cell
        specified by the index vector argument.

        [ extracts the atomic vectors associated with the cells
        specified by the index matrix argument.
    ',
    init=function(., data=NA, dim=data %|% `#`, n=1) {
        .$.NATTRIBUTES <- n
        .$.DIM <- dim
        .$.DATA <- array(data=data, dim=.$.NATTRIBUTES %,% dim)
        . },
    dim=function (.) .$.DIM,
    `[[`=function(., i)
            .$.DATA[
                .$.NATTRIBUTES %|% seq %cbind% rrep(i, .$.NATTRIBUTES)],
        #WAS eval(parse(text=".$.DATA[" %//% `%//%`(prefix(i, HCOMMA)) %//% "]")),
    `[`=function (., m)
            vapply_(
                m %|% rowNos,
                function(i) .$`[[`(., m[i,])),
    as.array=function(.) .$.DATA,
    .NULL=NULL )

