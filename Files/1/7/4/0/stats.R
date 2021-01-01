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


fn.ikk.permutation <- function (n, k)
        k %mod1% n

    Doc$fn.ikk.permutation <- '
        fn.ikk.permutation is intended for currying. The primary
        argument, n, is an integer-valued vector. There are
        length(n) items, each of which may take one of n[1],
        n[2], ...  values. The The curried function returns the
        k-th (arg 2 of fn.ikk.permutation) permutation of items.
        The return is a vector index representing the
        permutation of items.

        The order of permutations is like that of 

        > vapply_(1:12, fn.ikk.permutation %<=% c(3, 4))

             [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]

        [1,]    1    2    3    1    2    3    1    2    3     1     2     3

        [2,]    1    2    3    4    1    2    3    4    1     2     3     4
        '

permutation <- function(m, n, i) {
	i <- imod(i, m * n)
	imod(i, m) %,% idiv(i, m) }

    Doc$permutation <- '
        permutation returns the i-th (arg3) permutation two
        items, taking one from each of two ~ordered sets.~ of
        length m (arg1) and n (arg2). The return is an integer
        duple containig the indices of the first and second
        ~ordered sets.~ from which the permutation is formed. In
        computing the permutation, the items of the first
        ~ordered set~ are considered to be cycled through
        "faster." See permutations (with an ess for an
        example).
        
        HISTORY
        
        2019-06-05: Correction: second elmeent of return was
        previously computed incorrectly.'

permutations <- function(x, y)
cbind(
as.vector(matrix(x, nrow=length(x), ncol=length(y))),
as.vector(t(matrix(y, ncol=length(x), nrow=length(y)))))

combinations <- function (n, k=2) {
    if (k == 0) return (m.empty(nrow=1))
    if (k  < 1) return (m.empty())
    if (k == 1) return (1:n %|% matrix)
    if (2 < k) {
        warning("Does not yet implement k > 2")
        return (m.empty(nrow=choose(n,k))) }
    # k == 2
    mi.lower.triangular(n, offset=1) %|% cswap }

    Doc$combinations <- '
        combinations returns a matrix of indices, one row per
        combination of n items. As of 2019-05-06, implemented
        for k in 0:2 only.'


permutations.i <- function (i) {
    lg <- i %|% `#`
    if (i %|% is.empty) return (matrix(iNA, nrow=0, ncol=0))
    if (any(i < 1)) return (matrix(iNA, nrow=0, ncol=lg))
    n <- i %|% prod
    m <- matrix(iNA, nrow=n, ncol=lg)
    m[, 1] <- imod(1:n, i[1])
    for (k in i[-1] %|% seq_along %|% succ)
            m[,k] <- reprep(1:i[k], i[1:pred(k)] %|% prod)
    m }

permutations.i <- function (i)
        which(array(T, dim=i), arr.ind=T, useNames=F)

    Doc$permutations.i <- '
        permutations.i returns a matrix whose rows each specify
        one of every permutation of (arg 1)[1] items with (arg 1)[2]
        items with ... For example, permutations.i(rep2(2))
        could represent each of the values that can be
        represented by two bits.'

permutations.kk.n <- function (n)
    if (n < 1) m.empty() else
    if (n == 1) M1 else
    if (n == 2) tc(1L %,% 2L) else
    rrep(1:n, n) %m+v.% 0:(n-1) %mod1% n

    Doc$permutations.kk.n <- '
        permutations.kk.n returns a matrix of indices of n (arg
        1) unique items, one permutation per row, where the
        indices count up **** "MODULO" **** in either direction.

        The return might represent different orderings of the
        vertices of a polygon.

        The reverse-direction permutation are **** NOT
        REPRESENTED **** directly in the return. Apply rflip to
        the return to obtain the reverse permutations.
        
        If n is 12, the indices could represent the hours of the
        clock staring from any hour and going forward in time.'
