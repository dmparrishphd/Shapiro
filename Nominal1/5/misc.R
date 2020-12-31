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

unnull <- function(x=NULL, default=NA)
		if (x %|% is.null) default else x

    Doc$unnull <- '
        unnull returns arg 1 if it is not NULL. otherwise
        returns default (arg 2).'

`%st%` <- function(n, x) x[[n]]
`%nd%` <-`%st%`
`%rd%` <-`%st%`
`%th%` <-`%st%`

    Doc$`%st%` <- '
        `%st%`, `%nd%`, `%rd%`, and `%th%` are vector indexing
        functions, returning the item from the left argument
        that is specified by the right argument, using `[[`
        internally. The latter are aliases of the first.'

`%,%` <- function (x, y) c(x, y) # function

as_storage.mode <- function (mode, X) {
    storage.mode(X) <- mode;   X }

    Doc$as_storage.mode <- '
        as_storage.mode is a functional version of storage.mode.
        Intended use: curry the mode, apply the result of
        currying as a one-argument function.'

as_logical.storage.mode <- as_storage.mode %<=% "logical"

as_integer.storage.mode <- as_storage.mode %<=% "integer"

as_double.storage.mode <- as_storage.mode %<=% "double"

arrayInd.every <- function (.dim) #TAGS array index
        arrayInd(1:prod(.dim), .dim)
