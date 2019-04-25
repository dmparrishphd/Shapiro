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
runs.b <- function(b) {
    if (b  % %  is_empty) matrix(integer(), nrow=2) else {
        stops <- b  % %  xorr  % %  which %,% length(b)
        rbind(1L %,% succ(unrest(stops)), stops) % % as.matrix % % anonymize } }

        Doc$runs.b=crunch.h("
        Returns the runs found in the logical vector argument. A
        run is a sequence of identical values. The return is a 2
        x n matrix, 0 <= n, where n is the number of runs. The
        values within the matrix are the indices of the first
        (row 1) and last (row 2) elements within a run. Example:
        The call runs.b(c(F, T, T, F, F, F)) returns the
        equivalent of matrix(c(1, 1,   2, 3,   4, 6), nrow=2)")

run.lengths.b <- function(b)
    colply(b  % %  runs.b, diff.bak %O% (`+` %<=% 1L), integer(2))[2,]

        Doc$run.lengths.b=crunch.h("
        Returns, as an integer vector, the run lengths found in
        the logical vector argument")

'
function runs.b : run length encoding compression RLE

        See Doc$runs.b

function run.lengths.b : run length encoding compression RLE

        See Doc$run.lengths.b
'
