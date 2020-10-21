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
lg <- length

tp <- typeof

lss <- function (env=as.environment("ShapirEnv")) ls(env)

.gls <- function (h1, h2, value=T, ...)
        grep(h1, h2, value=value, ...)

glss <- function(h, ...) .gls(h, lss(), ...)
gls  <- function(h, ...) .gls(h, ls(globalenv()), ...)

'
function gls : ls search

        Searches the list of objects in the global environment
        for names which match the pattern (arg 1). Returns a
        character vector of the items matched. Additional
        arguments are passed to grep.

function glss : ls search

        Similar to gls, but searches ShapirEnv instead.
'
