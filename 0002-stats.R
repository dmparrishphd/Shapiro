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

permutation <- function(m, n, i) {
	i <- imod(i, m * n)
	imod(i, m) %,% idiv(i, n) }

    Doc$permutation <- '
        permutation returns the i-th (arg3) permutation two
        items, taking one from each of two ~ordered sets.~ of
        length m (arg1) and n (arg2). The return is an integer
        duple containig the indices of the first and second
        ~ordered sets.~ from which the permutation is formed. In
        computing the permutation, the items of the first
        ~ordered set~ are considered to be cycled through
        "faster." See permutations (with an ess for an
        example).'

permutations <- function(x, y)
cbind(
as.vector(matrix(x, nrow=length(x), ncol=length(y))),
as.vector(t(matrix(y, ncol=length(x), nrow=length(y)))))


