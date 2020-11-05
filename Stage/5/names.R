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

same.names <- function(X=list(), names.FUN=names) {
    L <- X %|% `#`
    if (    !L) return(logical())
    if (1 == L) return(rep(T, L))
    Names. <- lapply(X, names.FUN)
    capply(
        do.call(rbind, lapply(
            Names.,
            list(
                length.out=vapply_(Names., `#`) %|% max,
                values=HNULL) %v% extend.to.length)),
        equalr %O% all) }

    Doc$same.names <- '
        same.names returns a logical vector indicating which
        names are equal among the elements of the list argument.
        Typically, each list argument will be a named list or
        vector. The optional names.FUN argument may be set to
        row.names, etc.'
