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

reflexive <- function (FUN, x) FUN(x %|% rest, x %|% except.last)
.reflex <- reflexive #DEPRECATED use reflexive

    Doc$reflexive <- '
        reflexive applies FUN (arg 1), a function of two vectors
        of equal length, to the rest of the vector argument and
        to the initial part of the vector (all except the last
        element).

        reflexive is intended for currying. For example, see
        xorr (and its definition), which can be used as a simple
        edge detection function.'

l.multifunction <- function (FUNs)
    function(X) lapply(
        X,
        function(x) lapply(
            FUNs,
            `%|%` %<=% x))

multifunction <- function (FUNs)
    function(X) vapply(
        X,
        function(x) vapply(
            FUNs,
            `%|%` %<=% x,
            1 %|% double),
        FUNs %|% `#` %|% double)

swap <- function (x)
        if (x %|% `#` < 2) x else x[2:1] %,% x[2 %|% sans]

    Doc$swap <- '
        swap returns a copy of the vector argument with its
        first two elements swapped.'
