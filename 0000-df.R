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

dfapply <- function (df1, FUN, FUN.VALUE=NULL, ..., USE.NAMES=T) {
    #FUTURE: AUTOCOMPUTE FUN.VALUE IF NOT GIVEN
    #return. <- vector(mode=FUN.VALUE %|% typeof, length=df1 %|% nrow)
    vapply(
        df1 %|% nrow %|% seq,
        function(i) do.call(FUN, df1[i,]),
        FUN.VALUE) }
#dfapply(iris, function(...)..2, 1)

df.initialized <- function(nrows, colClasses, stringsAsFactors=F)
		data.frame(
			lapply(colClasses, function(cc) vector(mode=cc, length=nrows)),
			fix.empty.names=F,
			stringsAsFactors=stringsAsFactors)  % %
        rename.all

    Doc$df.initialized <- ('
        df.initialized returns a data frame of nrows rows with
        columns of the modes specified by colClasses. Values are
        initialized to the default value associated with vector.
        Cf. data.frame and read.table.')
