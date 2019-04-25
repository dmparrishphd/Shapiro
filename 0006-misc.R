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

i.near <- function (x, table, compar=`<=`) {
    compar. <- m.FUN.m..v(compar)
    compar.(-Inf %rbind% m.rep(table %|% matrix, x %|% `#`) %rbind% Inf, x) %|%
            (capply %|% argswap %<=% i.edge1.b) - 1L
}

    Doc$i.near <- '
        i.near returns the initial indices of table (arg 2) at
        which there is a transition (i.e., from TRUE to FALSE or
        vice versa) in the resuslt of comparrison between values
        in x (arg 1) and values in the table.

        Originally intended as a kind of lookup; table is a
        vector of increasing values.

        The inequality
        table[i.near(x, table)] <= x < table[1 + i.near(x, table)] 
        should hold for any single number x and any numeric
        table, where the table has hypothetical values at index
        zero and index 1 + length(table).
        
        Cf. look, match.

        ________
        * An alternate comparrison may be specified by the
        optional FUN argument.
        '

.i.nearest <- function (x, table) {
    i <- .i.near(x, table)
    if (!i) return (1L)
    if (i == table  % %  `#`) return (table  % %  `#`)
    tbl <- table[c(i, 1 + i)]
    (1:2)[diff.abs(tbl, x)  % %  which.min] }

i.nearest <- function (x, table)
        vapply(x, .i.nearest, 1L, table)

    Doc$i.nearest <- crunch.h('
        i.nearest returns the indices of the values in table
        (arg 2, a sorted vector of unique values) which are
        nearest each element of x (arg 1).  Cf. match.')

.nearest <- function (x, table) {
    i <- .near(x, table)
    if (!i) return (table  % %  first)
    if (i == table  % %  `#`) return (table  % %  last)
    tbl <- table[c(i, 1 + i)]
    tbl[diff.abs(tbl, x)  % %  which.min] }

nearest <- function (x, table)
        vapply(x, .nearest, .nearest(x[1], table), table)

    Doc$nearest <- crunch.h('
        nearest returns the values in table (arg 2, a sorted
        vector of unique values) which are nearest each element
        of x (arg 1).  Cf. match.')

