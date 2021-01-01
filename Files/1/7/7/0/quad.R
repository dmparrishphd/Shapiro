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

# VARIOUS FUNCTIONS RELATED TO QUADTREES

matrix.square <- function (nrow=1, ...)
        matrix(..., nrow=nrow, ncol=nrow)

    Doc$matrix.square <- '
        matrix.square returns a square matrix. It is a wrapper
        for matrix. All arguments are passed to matrix. However,
        ncol should not be specified, as it is fixed to nrow.'

.as_square.matrix <- function (m, N) {
    mm <- matrix.square(data=m %|% typeof %|% na, nrow=N)
    mm[m %|% rowNos, m %|% colNos] <- m
    mm }

as_square.matrix <- function (m) {
    NR <- m %|% nrow
    NC <- m %|% ncol
    if (NC == NR) m else
            .as_square.matrix(m, max(NR, NC)) }

    Doc$as_square.matrix <- '
        as_square.matris returns a square matrix having
        dimensions that match the longest dimension of the
        matrix argument. The "upper left" portion of the return
        is equal to the argument.'

as_quad.matrix <- function (m) {
    NR <-  2 ^ decade(m %|% nrow, 2)
    NC <-  2 ^ decade(m %|% ncol, 2)
    if (NR == m %|% nrow && NC == m %|% ncol) m else
            .as_square.matrix(m, max(NR, NC)) }

    Doc$as_quad.matrix <- '
        as_quad.matrix is much the same as as_square.matrix,
        except that the return has a dimension that is a power
        of 2. Originally intended for use in working with
        quadtrees. REF: https://en.wikipedia.org/wiki/Quadtree.'

ij.quadrant.ij <- function (ij, i=1, j=1)
        list(   nibble(ij %|% firstc, n=i),
                nibble(ij %|% secondc, n=j)   )

ij.quadrant <- function (m, i=1, j=1)
        list(   nibble(m %|% rowNos, n=i),
                nibble(m %|% colNos, n=j)   )

extract.quadrant <- function (m, i=1, j=1) {
    ij <- ij.quadrant(m, i=i, j=j)
    m[  ij[[1]], ij[[2]] ] }

assign.quadrant <- function (m, x, i=1, j=1) {
    ij <- ij.quadrant(m, i=i, j=j)
    m[  ij[[1]], ij[[2]] ]  <- x
    m }

as_quad.m <- function (m) {
    max()
    NROW <- m %|% nrow %\% 4L * 4L
    NCOL <- m %|% ncol %\% 4L * 4L
     }
